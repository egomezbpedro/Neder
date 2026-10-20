;;; neder.el --- Port of ThePrimeagen's 99 from Neovim -*- lexical-binding: t; -*-

;; Author: ddx
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.2"))
;; Keywords: convenience, tools, ai
;; URL: https://github.com/egomezbpedro/Neder
;; License: MIT

;;; Commentary:
;; This package opinionatedly ports the 99 agent CLI orchestrator to Emacs.
;;
;; Neder is not an HTTP client.  It spawns an agentic coding CLI (claude,
;; opencode, ...) as a subprocess, hands it a prompt assembled from the
;; current selection plus project context files, and applies the result
;; back to the buffer.
;;
;; Operations are entries in a registry (`neder-register-op'), so a new one
;; is data rather than code.  Three ship by default:
;;
;;   `neder-visual' - replace the active region with the model's output.
;;   `neder-ask'    - answer a question about the region, touching nothing.
;;   `neder-search' - project-wide agentic search into a `grep-mode' buffer.
;;
;; A note on how the response is retrieved, because it is the least obvious
;; part of the upstream design.  Upstream tells the CLI to write its answer
;; to a temp file and reads that file back; stdout is used only to render
;; progress.  That requires the CLI to have file-write permission.  Since
;; `neder-unsafe-skip-permissions' defaults to nil here, the safe path is
;; instead a structured stdout format: providers declare a `parse-response'
;; that turns the CLI's stdout into the answer text plus metadata, so the
;; answer is never recovered by guessing at prose.  See
;; `neder--retrieve-response'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'grep)
(require 'ansi-color)

(declare-function project-root "project")
(declare-function project-current "project")
(declare-function treesit-parser-list "treesit")
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-parent-until "treesit")
(declare-function flymake-diagnostics "flymake")
(declare-function flymake-diagnostic-text "flymake")
(declare-function flymake-diagnostic-type "flymake")
(declare-function flymake-diagnostic-beg "flymake")
(declare-function eglot-current-server "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function jsonrpc-request "jsonrpc")
(declare-function ediff-buffers "ediff")
(defvar evil-state)
(defvar evil-visual-selection)
(defvar ediff-buffer-B)
(defvar ediff-quit-hook)


;;;; Customization

(defgroup neder nil
  "Orchestrate agentic coding CLIs from Emacs."
  :group 'tools
  :prefix "neder-")

(defcustom neder-provider 'claude
  "Provider used for requests.
Must be a key in `neder--providers'.  See `neder-register-provider'."
  :type '(choice (const claude) (const opencode) symbol)
  :group 'neder)

(defcustom neder-model nil
  "Model passed to the provider CLI.
When nil, the provider's default model is used.  The value is captured
on the request when it is created, so changing it mid-flight does not
affect requests already running."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'neder)

(defcustom neder-executable-overrides nil
  "Alist of (PROVIDER-NAME . PROGRAM) overriding a provider's executable.

PROGRAM may be an absolute path, which is the reliable choice when Emacs
was started from a desktop launcher and so did not inherit the login
shell's PATH:

  (setq neder-executable-overrides
        \\='((claude . \"~/.local/bin/claude\")))"
  :type '(alist :key-type symbol :value-type string)
  :group 'neder)

(defcustom neder-provider-extra-args nil
  "Extra arguments appended to the provider command line.
Upstream appends these after the prompt argument, and so do we."
  :type '(repeat string)
  :group 'neder)

(defcustom neder-unsafe-skip-permissions nil
  "When non-nil, let the provider CLI act without asking for permission.

For the claude provider this passes `--dangerously-skip-permissions',
which lets the CLI create, modify and delete files and run shell
commands with no confirmation.

This also switches how the response is transported.  When non-nil, the
prompt instructs the CLI to write its answer to a temp file, matching
upstream.  When nil, the prompt instructs it to answer on stdout, since
a CLI without write permission cannot produce the temp file.

The opencode provider has no equivalent command-line flag, so for it
this option only affects the prompt.  Note that `opencode run --agent
build' can modify files regardless of this setting."
  :type 'boolean
  :group 'neder)

(defcustom neder-context-file-names nil
  "File names collected while walking up from the current file.
Every match found between the current file's directory and the project
root is included, nearest last.  Upstream defaults to the empty list,
meaning no context files are read; a useful value is
\(\"CLAUDE.md\" \"AGENTS.md\")."
  :type '(repeat string)
  :group 'neder)

(defcustom neder-context-strategy 'defun
  "How the context surrounding a region is chosen.

`lines' takes `neder-surrounding-context-lines' above and below, which is
what upstream does.

`defun' takes the enclosing function, method or class, via tree-sitter
when the buffer has a parser and `beginning-of-defun' otherwise, and
falls back to `lines' when neither finds an enclosing form.  This costs
fewer tokens and, more importantly, never hands the model a function cut
in half, which reads as a syntax error rather than as a window."
  :type '(choice (const lines) (const defun))
  :group 'neder)

(defcustom neder-surrounding-context-lines 100
  "Lines of context included above and below the region.
Used when `neder-context-strategy' is `lines', and as the fallback when
`defun' finds no enclosing form.  Hardcoded to 100 upstream."
  :type 'integer
  :group 'neder)

(defcustom neder-include-diagnostics t
  "When non-nil, include Flymake diagnostics overlapping the region.

These are already computed and sitting in the buffer, so this is the
cheapest high-signal context available: it tells the model what the
language server already knows is wrong with the code it is being asked
to change."
  :type 'boolean
  :group 'neder)

(defcustom neder-include-hover nil
  "When non-nil, include LSP hover text for symbols in the region.

Off by default: each symbol costs a synchronous `jsonrpc-request' to the
language server, so a large region can block Emacs for a noticeable
moment.  Worth turning on for small, type-heavy selections."
  :type 'boolean
  :group 'neder)

(defcustom neder-hover-symbol-limit 12
  "Maximum number of symbols to request hover text for."
  :type 'integer
  :group 'neder)

(defcustom neder-hover-timeout 0.4
  "Seconds to wait for each LSP hover response before giving up."
  :type 'number
  :group 'neder)

(defcustom neder-skill-directories
  '(".agents/skills/" ".claude/skills/" "~/.claude/skills/")
  "Directories searched for skills, in precedence order.

A skill is a directory containing a SKILL.md, so a directory listed here
is expected to hold <skill-name>/SKILL.md.  Relative entries resolve
from the project root, which is why the project-local directories come
first: a repo's own skill of a given name shadows the global one.

Reference a skill from a prompt by writing #<name>.  Port of the
`custom_rules' option and `99.extensions.agents' upstream."
  :type '(repeat string)
  :group 'neder)

(defcustom neder-reference-files t
  "When non-nil, resolve @path references in the prompt to file contents.
Paths resolve from the project root.  Port of `99.extensions.files'."
  :type 'boolean
  :group 'neder)

(defcustom neder-max-reference-file-bytes 102400
  "Skip @path references to files larger than this many bytes.
Matches upstream's `max_file_size' default."
  :type 'integer
  :group 'neder)

(defcustom neder-use-prompt-buffer t
  "When non-nil, read prompts in a `neder-prompt-mode' buffer.

The buffer is what makes #skill and @file references usable: a
`read-string' minibuffer has no completion for them and no room for a
prompt longer than a line.  Set to nil to go back to the minibuffer."
  :type 'boolean
  :group 'neder)

(defcustom neder-stdout-rows 3
  "How many trailing lines of CLI output to show while a request runs."
  :type 'integer
  :group 'neder)

(defcustom neder-spinner t
  "When non-nil, show an in-buffer spinner on the region while running.
Upstream draws a throbber at both ends of the selection; this is the
overlay analogue, and it survives the echo area being busy."
  :type 'boolean
  :group 'neder)

(defcustom neder-spinner-frames ["|" "/" "-" "\\"]
  "Frames cycled by the in-buffer spinner."
  :type '(vector :tag "Frames" string string string string)
  :group 'neder)

(defcustom neder-temp-directory nil
  "Directory for response temp files, used only in unsafe mode.
When nil, `.neder-tmp' under the project root is used.  The directory must
live inside the project, or the provider CLI will refuse to write to it."
  :type '(choice (const :tag "<project-root>/.neder-tmp" nil) directory)
  :group 'neder)

(defcustom neder-request-timeout 300
  "Seconds before an in-flight request is cancelled, or nil for no limit.
Upstream has no timeout."
  :type '(choice (const :tag "No timeout" nil) number)
  :group 'neder)

(defcustom neder-max-concurrent-requests 3
  "Maximum number of simultaneous in-flight requests.
Upstream has no limit."
  :type 'integer
  :group 'neder)

(defcustom neder-kill-grace-seconds 3
  "Seconds to wait after SIGTERM before sending SIGKILL.
Upstream sends only SIGTERM."
  :type 'number
  :group 'neder)

(defcustom neder-visual-indent t
  "When non-nil, reindent the replacement inserted by `neder-visual'."
  :type 'boolean
  :group 'neder)

(defcustom neder-visual-apply-style 'confirm
  "How `neder-visual' applies the model's answer to the buffer.

`direct'  replaces the region immediately.
`confirm' shows a unified diff of the replacement and asks first.
`ediff'   opens an Ediff session between the original and the answer,
          and applies whatever is in the answer buffer when it ends,
          so the answer can be edited or partially accepted.

`confirm' is the default: an agentic CLI writing straight into a source
file with no review is the one failure mode that costs real work."
  :type '(choice (const direct) (const confirm) (const ediff))
  :group 'neder)

(defcustom neder-confirm-on-changed-selection t
  "When non-nil, ask before replacing a region that changed mid-request.

The model's answer was computed from the text as it was when the request
started.  If that text has since been edited, applying the answer
silently discards the edit.  Markers alone cannot catch this: they track
the edit perfectly and stay valid, so the only signal is the text
itself."
  :type 'boolean
  :group 'neder)

(defcustom neder-debug nil
  "When non-nil, log to `*neder-log*' and mirror each prompt to a file.
The mirror is written under `temporary-file-directory'."
  :type 'boolean
  :group 'neder)


;;;; Internal state

(cl-defstruct (neder-provider (:constructor neder-provider-create))
  "A provider wraps one agentic coding CLI."
  name
  display-name
  executable
  default-model
  ;; Called with (MODEL PROMPT UNSAFE-P) and returns the argv list.
  build-command
  ;; Called with the accumulated stdout string.  Returns a plist with at
  ;; least :text, and optionally :cost, :session-id, :turns and :error.
  ;; When nil, stdout is taken as the answer verbatim.
  parse-response
  ;; A list of model name strings, or a function of one callback argument.
  models)

(cl-defstruct (neder-op (:constructor neder-op-create))
  "One registered operation.

REGION-P says whether the op reads the active region.  TRACK-P says
whether its answer is applied back to that region, which is what decides
if markers are set.  OP-PROMPT is called with the request and returns
the operation text that goes in the <Context> block.  COMPLETE is called
with the request and the response text."
  name
  region-p
  track-p
  op-prompt
  complete)

(cl-defstruct (neder-request (:constructor neder--request-create))
  "One in-flight or completed request."
  id
  op                                    ; a key in `neder--ops'
  (state 'ready)                        ; ready|requesting|success|failed|cancelled
  provider
  model
  buffer
  directory
  user-prompt
  context                               ; list of strings, reverse order
  prompt-string
  temp-file
  stdout                                ; list of chunks, reverse order
  stderr                                ; list of chunks, reverse order
  process
  timer
  kill-timer
  start-marker
  end-marker
  original-length                       ; span length when the request started
  original-text                         ; span text when the request started
  selection                             ; selected text, for rendering
  location                              ; file:line:col-line:col, for rendering
  surrounding                           ; context block around the selection
  extra-context                         ; diagnostics/hover blocks, a string
  major-mode-name                       ; source mode, for the org src block
  overlay                               ; in-buffer spinner overlay
  spinner-timer
  (spinner-index 0)
  pending-apply                         ; non-nil while an async apply is open
  result-buffer                         ; buffer holding this request's output
  cost                                  ; total_cost_usd, when reported
  session-id
  turns
  settled                               ; non-nil once the sentinel has run
  error)

(defvar neder--providers nil
  "Alist of (NAME . `neder-provider') for every registered provider.")

(defvar neder--ops nil
  "Alist of (NAME . `neder-op') for every registered operation.")

(defvar neder--requests nil
  "List of all requests this session, newest first.")

(defvar neder--request-counter 0
  "Monotonic request id counter.")

(defun neder-register-provider (provider)
  "Register PROVIDER, replacing any provider with the same name.

Adding one of the remaining upstream providers takes only a
`build-command' and a `default-model'.  The prompt is always the last
positional argument and `neder-provider-extra-args' is appended after it,
so a new provider is a one-liner:

  (neder-register-provider
   (neder-provider-create
    :name \\='cursor-agent
    :display-name \"CursorAgentProvider\"
    :executable \"cursor-agent\"
    :default-model \"sonnet-4.5\"
    :build-command
    (lambda (model prompt unsafe)
      (append \\='(\"cursor-agent\")
              (when unsafe \\='(\"--trust\" \"--force\"))
              (list \"--model\" model \"--print\" prompt)))))"
  (setf (alist-get (neder-provider-name provider) neder--providers) provider)
  provider)

(defun neder-register-op (op)
  "Register OP, replacing any operation with the same name."
  (setf (alist-get (neder-op-name op) neder--ops) op)
  op)

(defun neder--provider ()
  "Return the configured provider struct, or signal an error."
  (or (alist-get neder-provider neder--providers)
      (user-error "neder: unknown provider `%s'" neder-provider)))

(defun neder--op (name)
  "Return the `neder-op' registered as NAME, or signal an error."
  (or (alist-get name neder--ops)
      (error "neder: unknown op %s" name)))

(defun neder--active-requests ()
  "Return the requests whose state is `requesting'."
  (seq-filter (lambda (r) (eq (neder-request-state r) 'requesting))
              neder--requests))

(defun neder--log (format-string &rest args)
  "Append FORMAT-STRING formatted with ARGS to `*neder-log*' when debugging."
  (when neder-debug
    (with-current-buffer (get-buffer-create "*neder-log*")
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] ")
              (apply #'format format-string args)
              "\n"))))


;;;; Utilities

(defun neder--project-root ()
  "Return the project root for the current buffer.

Upstream anchors everything to the editor's cwd; in Emacs
`default-directory' is per-buffer, so the project root is the faithful
analogue.  `project-current' is consulted before `vc-root-dir' so that a
subproject inside a monorepo scopes requests to the subproject rather
than to the whole git checkout."
  (or (ignore-errors
        (when-let* ((p (project-current nil)))
          (project-root p)))
      (ignore-errors (vc-root-dir))
      default-directory))

(defun neder--blank-p (string)
  "Return non-nil when STRING is nil or contains only whitespace."
  (or (null string) (string-blank-p string)))

(defun neder--string-or-nil (string)
  "Return STRING, or nil when it is blank."
  (unless (neder--blank-p string) string))

(defun neder--unfence (string)
  "Strip a wrapping markdown code fence from STRING when it is the whole value.
Upstream has no such guard because its answers arrive via a file rather
than conversational output."
  (let ((trimmed (string-trim string)))
    (if (and (string-prefix-p "```" trimmed)
             (string-suffix-p "```" trimmed)
             (> (length trimmed) 6)
             ;; Only one fenced block: no closing fence in the middle.
             (not (string-match-p "^```[ \t]*$"
                                  (substring trimmed 3 (- (length trimmed) 3)))))
        (let* ((body (substring trimmed 3 (- (length trimmed) 3)))
               ;; Drop the info string on the opening fence.
               (nl (string-search "\n" body)))
          (string-trim (if nl (substring body (1+ nl)) body)))
      string)))

(defun neder--dedent (text)
  "Remove the common leading whitespace from every line of TEXT."
  (let* ((lines (split-string text "\n"))
         (indents (seq-keep
                   (lambda (line)
                     (unless (string-blank-p line)
                       (string-match "\\`[ \t]*" line)
                       (match-end 0)))
                   lines))
         (common (if indents (apply #'min indents) 0)))
    (string-join
     (mapcar (lambda (line)
               (if (> (length line) common) (substring line common)
                 (string-trim-left line)))
             lines)
     "\n")))

(defun neder--reindent (text indent)
  "Dedent TEXT, then prefix every line after the first with INDENT.

The first line is left bare because it is inserted at a point that
already sits at the selection's original indentation.

This replaces an `indent-region' call.  `indent-region' is a no-op in
modes whose indentation is ambiguous -- notably `python-mode', where it
silently leaves whatever indentation the CLI happened to emit."
  (if (string-empty-p indent)
      (neder--dedent text)
    (let ((lines (split-string (neder--dedent text) "\n")))
      (string-join
       (cons (car lines)
             (mapcar (lambda (line)
                       (if (string-blank-p line) line (concat indent line)))
                     (cdr lines)))
       "\n"))))

(defun neder--sanitize (string)
  "Strip ANSI sequences and an enclosing code fence from STRING."
  (when string
    (neder--string-or-nil
     (neder--unfence (ansi-color-filter-apply string)))))

(defun neder--temp-directory ()
  "Return the directory for response temp files, creating it if needed."
  (let ((dir (or neder-temp-directory
                 (expand-file-name ".neder-tmp" (neder--project-root)))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun neder--make-temp-file ()
  "Create and return an empty response temp file.
Upstream pre-creates the file too, so its existence never implies it has
content."
  (let ((file (expand-file-name (make-temp-name "neder-")
                                (neder--temp-directory))))
    (write-region "" nil file nil 'silent)
    file))

(defvar-local neder--buffer-request-id nil
  "Id of the request whose output this buffer holds.")

(defun neder--rename-stale-buffer (name)
  "Rename an existing buffer NAME out of the way, tagging it with its id.

Keeps the previous answer or search result reachable instead of erasing
it, while leaving NAME free for the newest request.  This is what makes
`neder-open' worth having: without it every op destroys its own history."
  (when-let* ((buf (get-buffer name)))
    (with-current-buffer buf
      (rename-buffer (format "%s<%s>*"
                             (string-trim-right name "\\*")
                             (or neder--buffer-request-id "prev"))
                     t))))


;;;; Prompt text

(defconst neder--prompt-template "<Context>
%s
</Context>
<Prompt>
%s
</Prompt>
"
  "Wrapper for the operation text and the user's text.
Port of `prompts.prompt' in lua/99/prompt-settings.lua.  Note that the
operation text goes in <Context> and the user's text in <Prompt>.")

(defconst neder--prompt-visual "You receive a selection in a text editor that you need to replace with new code.
The selection's contents may contain notes, incorporate the notes every time if there are some.
consider the context of the selection and what you are suppose to be implementing
<LANGUAGE>%s</LANGUAGE>
<SELECTION_LOCATION>
%s
</SELECTION_LOCATION>
<SELECTION_CONTENT>
%s
</SELECTION_CONTENT>
<SURROUNDING_CONTEXT>
%s
</SURROUNDING_CONTEXT>
"
  "Operation text for `neder-visual'.
Port of `prompts.visual_selection'.  Upstream says \"in neovim\"; saying
that from Emacs risks biasing the model toward vimscript.  The language
tag is an addition: otherwise the model infers the language from the
code text alone, which goes wrong on short selections.")

(defconst neder--prompt-ask "You receive a selection from a file and a question about it.
Answer the question.  Do not rewrite the code and do not propose edits
unless the question explicitly asks for them.
<Rule>The response must be valid Org mode markup, not Markdown</Rule>
<Rule>Use Org headings, never # headings</Rule>
<Rule>Headings must start at level two (two asterisks) or deeper, never level one, because the answer is placed under an existing level-one heading</Rule>
<Rule>Use Org emphasis: /italic/, *bold*, =code=, ~verbatim~</Rule>
<Rule>Wrap code in #+begin_src LANG ... #+end_src blocks, never in triple backticks</Rule>
<Rule>Use - for list items</Rule>
<Rule>Do not include a top-level title; the first heading must be a section of the answer</Rule>
<SELECTION_LOCATION>
%s
</SELECTION_LOCATION>
<SELECTION_CONTENT>
%s
</SELECTION_CONTENT>
<SURROUNDING_CONTEXT>
%s
</SURROUNDING_CONTEXT>
"
  "Operation text for `neder-ask'.
Non-destructive: the answer is rendered in a separate Org buffer rather
than replacing the selection.  Upstream has no equivalent operation.")

(defconst neder--prompt-search "<Output>
/path/to/project/src/foo.js:24:8,3,Some notes here about some stuff, it can contain commas
/path/to/project/src/foo.js:71:12,7,more notes, everything is great!
/path/to/project/src/bar.js:13:2,1,more notes again, this time specfically about bar and why bar is so important
/path/to/project/src/baz.js:1:1,52,Notes about why baz is very important to the results
</Output>
<Rule>Text locations are in the format of: /path/to/file.ext:lnum:cnum,X,NOTES
lnum = starting line number 1 based
cnum = starting column number 1 based
X = how many lines should be highlighted
NOTES = A text description of why this highlight is important

See <Output> for example
</Rule>
<Rule>NOTES cannot have new lines</Rule>
<Rule>You must adhere to the output format</Rule>
<Rule>Double check output format before writing it</Rule>
<Rule>Each location is separated by new lines</Rule>
<Rule>Each path is specified in absolute pathing</Rule>
<Rule>You can provide notes you think are relevant per location</Rule>
<Rule>You must provide output without any commentary, just text locations</Rule>
<Example>
You have found 3 locations in files foo.js, bar.js, and baz.js.
There are 2 locations in foo.js, 1 in bar.js and baz.js.
<Meaning>
This means that the search results found
foo.js at line 24, char 8 and the next 2 lines
foo.js at line 71, char 12 and the next 6 lines
bar.js at line 13, char 2
baz.js at line 1, char 1 and the next 51 lines
</Meaning>
</Example>
<TaskDescription>
you are given a prompt and you must search through this project and return code that matches the description provided.
</TaskDescription>
"
  "Operation text for `neder-search'.
Port of `prompts.semantic_search'.")

(defconst neder--must-obey-temp-file "<MustObey>
NEVER alter any file other than TEMP_FILE.
never provide the requested changes as conversational output. Return only the code.
ONLY provide requested changes by writing the change to TEMP_FILE

never attempt to read TEMP_FILE.
It is purely for output.
Previous contents, which may not exist, can be written over without worry
After writing TEMP_FILE once you should be done.  Be done and end the session.
</MustObey>"
  "Verbatim port of `only_tmp_file_change', used in unsafe mode only.")

(defconst neder--must-obey-stdout "<MustObey>
Write your answer to standard output only.
NEVER create, modify, or delete any file.
Return only the requested content, with no commentary, no preamble, and
no markdown code fences.
</MustObey>"
  "Safe-mode replacement for `neder--must-obey-temp-file'.
The upstream block forbids conversational output, which would suppress
the stdout this mode depends on, so it must not be sent here.")


;;;; Skills and file references

(defun neder--skill-directories ()
  "Return `neder-skill-directories' resolved against the project root."
  (let ((root (neder--project-root)))
    (mapcar (lambda (dir)
              (file-name-as-directory
               (if (or (file-name-absolute-p dir) (string-prefix-p "~" dir))
                   (expand-file-name dir)
                 (expand-file-name dir root))))
            neder-skill-directories)))

(defun neder--skills ()
  "Return an alist of (NAME . SKILL.md-PATH) for every discoverable skill.

Earlier entries in `neder-skill-directories' win, so a project-local
skill shadows a global one of the same name.  Port of
`99.extensions.agents.rules'."
  (let (out)
    (dolist (dir (neder--skill-directories))
      (when (file-directory-p dir)
        (dolist (file (file-expand-wildcards
                       (expand-file-name "*/SKILL.md" dir)))
          (let ((name (file-name-nondirectory
                       (directory-file-name (file-name-directory file)))))
            (unless (assoc name out)
              (push (cons name file) out))))))
    (nreverse out)))

(defun neder--file-contents (path)
  "Return the contents of PATH, or nil when it is unreadable or too large."
  (when (and (file-readable-p path)
             (not (file-directory-p path))
             (< (or (file-attribute-size (file-attributes path)) 0)
                neder-max-reference-file-bytes))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun neder--prompt-tokens (prompt prefix)
  "Return the distinct tokens in PROMPT introduced by the PREFIX character."
  (let ((case-fold-search nil)
        (regexp (format "%s\\([^[:space:]]+\\)" (regexp-quote prefix)))
        (start 0)
        out)
    (while (string-match regexp prompt start)
      (let ((token (match-string 1 prompt)))
        (unless (member token out) (push token out)))
      (setq start (match-end 0)))
    (nreverse out)))

(defun neder--resolve-skills (prompt)
  "Return reference blocks for every #name in PROMPT that names a skill.
Each block is wrapped in a tag named after the skill, matching
`M.get_rule_content' upstream."
  (let ((skills (neder--skills)))
    (seq-keep
     (lambda (token)
       ;; Accept both #name and #path/to/SKILL.md, as upstream does.
       (let* ((entry (or (assoc token skills)
                         (rassoc (expand-file-name token (neder--project-root))
                                 skills)))
              (name (car-safe entry))
              (content (and entry (neder--file-contents (cdr entry)))))
         (when content
           (neder--log "skill %s from %s" name (cdr entry))
           (format "<%s>\n%s\n</%s>" name content name))))
     (neder--prompt-tokens prompt "#"))))

(defun neder--resolve-files (prompt)
  "Return reference blocks for every @path in PROMPT that names a file."
  (when neder-reference-files
    (let ((root (neder--project-root)))
      (seq-keep
       (lambda (token)
         (let* ((path (expand-file-name token root))
                (content (neder--file-contents path)))
           (when content
             (neder--log "reference file %s" path)
             (format "<File path=\"%s\">\n%s\n</File>" token content))))
       (neder--prompt-tokens prompt "@")))))

(defvar neder--project-files-cache nil
  "Cons of (ROOT . FILES) caching the last `neder--project-files' result.")

(defun neder--project-files ()
  "Return the project's tracked files, relative to the root.
Uses `git ls-files', which respects .gitignore for free, and falls back
to nothing outside a git checkout."
  (let ((root (neder--project-root)))
    (if (equal (car neder--project-files-cache) root)
        (cdr neder--project-files-cache)
      (let ((files (ignore-errors
                     (let ((default-directory root))
                       (process-lines "git" "ls-files")))))
        (setq neder--project-files-cache (cons root files))
        files))))


;;;; Context engine

(defun neder--read-context-files (start-dir root names)
  "Return the contents of every NAMES file between START-DIR and ROOT.

Walks upward from START-DIR to ROOT inclusive and collects *all* matches
at every level, nearest first in the walk.  This is why
`locate-dominating-file' is not used: it returns only the nearest hit
and would silently drop an outer CLAUDE.md, which is usually the one
carrying project-wide rules.

Port of `Prompt:_read_md_files' in lua/99/prompt.lua."
  (when (and names start-dir root)
    (let ((dir (file-name-as-directory (expand-file-name start-dir)))
          (root (file-name-as-directory (expand-file-name root)))
          (found nil))
      (catch 'done
        (while t
          (dolist (name names)
            (let ((path (expand-file-name name dir)))
              (when (file-readable-p path)
                (neder--log "context file %s" path)
                (push (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string))
                      found))))
          (when (equal dir root)
            (throw 'done nil))
          (let ((parent (file-name-directory (directory-file-name dir))))
            ;; Guard against spinning at "/" and against escaping the root.
            (when (or (null parent) (equal parent dir))
              (throw 'done nil))
            (setq dir (file-name-as-directory parent))
            (unless (file-in-directory-p dir root)
              (throw 'done nil)))))
      (nreverse found))))

(defun neder--region-location (file start-line start-col end-line end-col)
  "Render a range as FILE:START-LINE:START-COL-END-LINE:END-COL.
Columns are 1-based to match the convention the search prompt uses;
Emacs columns are 0-based, so callers add one."
  (format "%s:%d:%d-%d:%d" file start-line start-col end-line end-col))

(defconst neder--defun-node-regexp
  (rx (or "function" "method" "class" "declaration" "definition"
          "subroutine" "constructor" "struct" "impl"))
  "Node types treated as an enclosing form by `neder--treesit-bounds'.")

(defun neder--treesit-bounds (start end)
  "Return (FROM . TO) for the tree-sitter form enclosing START and END, or nil."
  (when (and (fboundp 'treesit-parser-list)
             (treesit-parser-list)
             (fboundp 'treesit-parent-until))
    (ignore-errors
      (when-let* ((leaf (treesit-node-at start))
                  (node (treesit-parent-until
                         leaf
                         (lambda (n)
                           (and (string-match-p neder--defun-node-regexp
                                                (treesit-node-type n))
                                ;; Must actually contain the whole region.
                                (<= (treesit-node-start n) start)
                                (>= (treesit-node-end n) end)))
                         t)))
        (cons (treesit-node-start node) (treesit-node-end node))))))

(defun neder--defun-bounds (start end)
  "Return (FROM . TO) for the defun enclosing START and END, or nil.

Falls back from tree-sitter to `beginning-of-defun'.  A result spanning
the whole buffer is rejected: in a mode with no notion of defuns, such
as `fundamental-mode' or `text-mode', `beginning-of-defun' happily
reports point-min to point-max, and calling that an enclosing form
would silently turn the context window into the entire file."
  (or (neder--treesit-bounds start end)
      (ignore-errors
        (save-excursion
          (goto-char start)
          (let ((from (progn (beginning-of-defun) (point)))
                (to (progn (goto-char start) (end-of-defun) (point))))
            (when (and (<= from start) (>= to end) (< from to)
                       (not (and (<= from (point-min)) (>= to (point-max)))))
              (cons from to)))))))

(defun neder--line-bounds (start end)
  "Return (FROM . TO) padded by `neder-surrounding-context-lines'."
  (save-excursion
    (cons (progn (goto-char start)
                 (forward-line (- neder-surrounding-context-lines))
                 (point))
          (progn (goto-char end)
                 (forward-line neder-surrounding-context-lines)
                 (line-end-position)))))

(defun neder--context-bounds (start end)
  "Return (FROM . TO . KIND) for the context around START and END."
  (let ((bounds (and (eq neder-context-strategy 'defun)
                     (neder--defun-bounds start end))))
    (if bounds
        (list (car bounds) (cdr bounds) "enclosing-defun")
      (let ((lines (neder--line-bounds start end)))
        (list (car lines) (cdr lines) "surrounding-lines")))))

(defun neder--surrounding-context (start end)
  "Return a cons of (TEXT . KIND) describing the context around START and END."
  (pcase-let ((`(,from ,to ,kind) (neder--context-bounds start end)))
    (cons (buffer-substring-no-properties from to) kind)))

(defun neder--diagnostics-block (start end)
  "Return a <DIAGNOSTICS> block for Flymake diagnostics touching START..END."
  (when (and neder-include-diagnostics
             (fboundp 'flymake-diagnostics))
    (when-let* ((diags (ignore-errors (flymake-diagnostics start end))))
      (format "<DIAGNOSTICS>\n%s\n</DIAGNOSTICS>"
              (string-join
               (mapcar (lambda (d)
                         (format "line %d [%s] %s"
                                 (line-number-at-pos (flymake-diagnostic-beg d))
                                 (flymake-diagnostic-type d)
                                 (flymake-diagnostic-text d)))
                       diags)
               "\n")))))

(defun neder--hover-at (server point)
  "Return the LSP hover string for POINT from SERVER, or nil."
  (ignore-errors
    (let* ((params (save-excursion
                     (goto-char point)
                     (eglot--TextDocumentPositionParams)))
           (res (jsonrpc-request server :textDocument/hover params
                                 :timeout neder-hover-timeout))
           (contents (plist-get res :contents))
           (value (cond ((stringp contents) contents)
                        ((plist-get contents :value))
                        ((vectorp contents)
                         (mapconcat (lambda (c)
                                      (if (stringp c) c
                                        (or (plist-get c :value) "")))
                                    contents "\n")))))
      (neder--string-or-nil value))))

(defun neder--hover-block (start end)
  "Return a <SYMBOLS> block of LSP hover text for symbols in START..END."
  (when (and neder-include-hover (fboundp 'eglot-current-server))
    (when-let* ((server (ignore-errors (eglot-current-server))))
      (let (seen out)
        (save-excursion
          (goto-char start)
          (while (and (< (point) end)
                      (< (length out) neder-hover-symbol-limit)
                      (forward-word 1)
                      (<= (point) end))
            (when-let* ((sym (thing-at-point 'symbol t)))
              (unless (member sym seen)
                (push sym seen)
                (when-let* ((hover (neder--hover-at server (point))))
                  (push (format "%s: %s" sym
                                (string-replace "\n" " " (string-trim hover)))
                        out))))))
        (when out
          (format "<SYMBOLS>\n%s\n</SYMBOLS>"
                  (string-join (nreverse out) "\n")))))))

(defun neder--extra-context (start end)
  "Return the diagnostics and hover blocks for START..END as one string."
  (neder--string-or-nil
   (string-join (delq nil (list (neder--diagnostics-block start end)
                                (neder--hover-block start end)))
                "\n")))

(defun neder--finalize-prompt (req &optional location-tag function-text)
  "Assemble and store the final prompt string on REQ.

LOCATION-TAG and FUNCTION-TEXT are the visual-only <Location> and
<FunctionText> blocks.  Order matches `Prompt:finalize' in
lua/99/prompt.lua: operation and user text first, then references, then
context files, then the location blocks, then the transport contract.
Context files deliberately land after the user prompt, and are inserted
raw with no wrapper tags, as upstream does.

References are the #skill and @file blocks resolved out of the user's
prompt, and sit between the two, which is where upstream puts them."
  (let ((parts (nreverse (copy-sequence (neder-request-context req))))
        (prompt (or (neder-request-user-prompt req) "")))
    ;; Extra context blocks (diagnostics, hover) computed at request time.
    (when-let* ((extra (neder-request-extra-context req)))
      (setq parts (append parts (list extra))))
    ;; References: #skills and @files.
    (setq parts (append parts
                        (neder--resolve-skills prompt)
                        (neder--resolve-files prompt)))
    ;; Context files.
    (setq parts
          (append parts
                  (neder--read-context-files
                   (when-let* ((buf (neder-request-buffer req))
                               ((buffer-live-p buf))
                               (file (buffer-file-name buf)))
                     (file-name-directory file))
                   (neder-request-directory req)
                   neder-context-file-names)))
    (when location-tag (setq parts (append parts (list location-tag))))
    (when function-text (setq parts (append parts (list function-text))))
    (if neder-unsafe-skip-permissions
        (setq parts (append parts
                            (list (format "<TEMP_FILE>%s</TEMP_FILE>"
                                          (neder-request-temp-file req))
                                  neder--must-obey-temp-file)))
      (setq parts (append parts (list neder--must-obey-stdout))))
    (setf (neder-request-prompt-string req) (string-join parts "\n"))
    (neder--mirror-prompt req)
    (neder-request-prompt-string req)))

(defun neder--mirror-prompt (req)
  "Write REQ's prompt to a debug file when `neder-debug' is non-nil.
Upstream mirrors this next to the response temp file; we keep it out of
the project so it never shows up as an untracked file."
  (when neder-debug
    (let ((file (expand-file-name
                 (format "neder-%d-prompt.txt" (neder-request-id req))
                 temporary-file-directory)))
      (write-region (neder-request-prompt-string req) nil file nil 'silent)
      (neder--log "prompt mirrored to %s" file))))

(defun neder--push-context (req string)
  "Push STRING onto REQ's context accumulator."
  (push string (neder-request-context req)))

(defun neder--op-prompt (op-text user-prompt)
  "Wrap OP-TEXT and USER-PROMPT in the <Context>/<Prompt> template."
  (when (neder--blank-p user-prompt)
    (user-error "neder: you must add a prompt to your request"))
  (format neder--prompt-template op-text user-prompt))


;;;; Providers

(defconst neder--claude-models
  '("claude-opus-5-5"
    "claude-sonnet-5-5"
    "claude-haiku-5-5"
    "claude-fable-5-1"
    "claude-sonnet-4-5"
    "claude-opus-4-1"
    "opus"
    "sonnet"
    "haiku")
  "Model names for the claude provider.
The claude CLI cannot list models, so upstream hardcodes them too, which
means this list goes stale rather than failing loudly.  The aliases at
the end always resolve to the current generation.")

(defun neder--claude-parse-response (stdout)
  "Parse claude's `--output-format json' STDOUT into a response plist.

The JSON envelope is why safe mode is trustworthy: the answer arrives in
a named field rather than having to be recovered from prose, so a
preamble the model decided to write can never be inserted into a source
buffer.  When the payload does not parse, STDOUT is returned verbatim so
a provider misconfiguration degrades instead of losing the answer."
  (let ((obj (ignore-errors
               (json-parse-string (string-trim (or stdout ""))
                                  :object-type 'alist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil))))
    (if-let* (((consp obj))
              (result (alist-get 'result obj)))
        (list :text result
              :cost (alist-get 'total_cost_usd obj)
              :session-id (alist-get 'session_id obj)
              :turns (alist-get 'num_turns obj)
              ;; is_error means the CLI reported failure with exit 0 and
              ;; put the message in `result'.
              :error (and (alist-get 'is_error obj) result))
      (list :text stdout))))

(neder-register-provider
 (neder-provider-create
  :name 'claude
  :display-name "ClaudeCodeProvider"
  :executable "claude"
  :default-model "claude-sonnet-5-5"
  :models neder--claude-models
  :parse-response #'neder--claude-parse-response
  :build-command
  (lambda (model prompt unsafe)
    ;; Port of OpenCodeProvider's sibling in lua/99/providers.lua, with
    ;; --dangerously-skip-permissions made conditional and the output
    ;; framed as JSON so the answer is a field, not a guess.
    (append '("claude")
            (when unsafe '("--dangerously-skip-permissions"))
            (list "--model" model
                  "--output-format" "json"
                  "--print" prompt)))))

(neder-register-provider
 (neder-provider-create
  :name 'opencode
  :display-name "OpenCodeProvider"
  :executable "opencode"
  :default-model "opencode/claude-sonnet-4-5"
  :models (lambda (callback)
            ;; `opencode models' lists them; unused until a model picker
            ;; exists, kept so that command is recorded somewhere.
            (funcall callback nil))
  ;; No structured output mode, so stdout is the answer verbatim.
  :parse-response nil
  :build-command
  (lambda (model prompt _unsafe)
    ;; opencode has no permission flag upstream, so UNSAFE is ignored here.
    (list "opencode" "run" "--agent" "build" "-m" model prompt))))

(defun neder--executable (provider)
  "Return the program to run for PROVIDER, or nil when it cannot be found.
An override from `neder-executable-overrides' wins, and an absolute path
is accepted directly so this works even when Emacs did not inherit the
login shell's PATH."
  (let* ((override (alist-get (neder-provider-name provider)
                              neder-executable-overrides))
         (program (or override (neder-provider-executable provider))))
    ;; Test the string as written: `expand-file-name' makes everything look
    ;; absolute, so a bare "claude" must still go through `executable-find'.
    (if (or (file-name-absolute-p program) (string-prefix-p "~" program))
        (let ((path (expand-file-name program)))
          (and (file-executable-p path) path))
      (executable-find program))))

(defun neder--build-command (provider prompt &optional model)
  "Return the argv list for PROVIDER and PROMPT, using MODEL.
`neder-provider-extra-args' is appended after the prompt, matching the
upstream `vim.list_extend' call."
  (append (funcall (neder-provider-build-command provider)
                   (or model neder-model (neder-provider-default-model provider))
                   prompt
                   neder-unsafe-skip-permissions)
          neder-provider-extra-args))


;;;; In-buffer spinner

(defun neder--spinner-start (req)
  "Attach a spinner overlay to REQ's tracked region."
  (when (and neder-spinner
             (markerp (neder-request-start-marker req))
             (buffer-live-p (neder-request-buffer req)))
    (with-current-buffer (neder-request-buffer req)
      (let ((ov (make-overlay (neder-request-start-marker req)
                              (neder-request-start-marker req)
                              nil t nil)))
        (overlay-put ov 'neder-request (neder-request-id req))
        (setf (neder-request-overlay req) ov)
        (setf (neder-request-spinner-timer req)
              (run-at-time 0 0.15 #'neder--spinner-tick req))))))

(defun neder--spinner-tick (req)
  "Advance REQ's spinner one frame."
  (let ((ov (neder-request-overlay req)))
    (if (not (and ov (overlay-buffer ov)
                  (eq (neder-request-state req) 'requesting)))
        (neder--spinner-stop req)
      (let* ((i (mod (cl-incf (neder-request-spinner-index req))
                     (length neder-spinner-frames)))
             (frame (aref neder-spinner-frames i)))
        (overlay-put ov 'before-string
                     (propertize (format "%s neder[%d] " frame
                                         (neder-request-id req))
                                 'face 'shadow))))))

(defun neder--spinner-stop (req)
  "Remove REQ's spinner overlay and timer."
  (when-let* ((timer (neder-request-spinner-timer req)))
    (cancel-timer timer)
    (setf (neder-request-spinner-timer req) nil))
  (when-let* ((ov (neder-request-overlay req)))
    (delete-overlay ov)
    (setf (neder-request-overlay req) nil)))


;;;; Process layer

(defvar neder--progress-last 0
  "Time of the last progress message, for throttling.")

(defun neder--show-progress (req)
  "Show the trailing output lines of REQ, throttled to four times a second."
  (let ((now (float-time)))
    (when (> (- now neder--progress-last) 0.25)
      (setq neder--progress-last now)
      (let* ((text (ansi-color-filter-apply
                    (apply #'concat (reverse (neder-request-stdout req)))))
             (lines (seq-remove #'string-blank-p (split-string text "\n")))
             (tail (last lines (max 1 neder-stdout-rows))))
        (message "neder[%d] %s" (neder-request-id req)
                 (string-join (mapcar #'string-trim tail) " / "))))))

(defun neder--process-filter (req)
  "Return a process filter closing over REQ."
  (lambda (_proc chunk)
    ;; Upstream checks for cancellation at the top of every callback.
    (when (eq (neder-request-state req) 'requesting)
      ;; Accumulating stdout is what makes safe mode work at all.
      (push chunk (neder-request-stdout req))
      (neder--show-progress req))))

(defun neder--stderr-filter (req)
  "Return a stderr filter closing over REQ."
  (lambda (_proc chunk)
    (push chunk (neder-request-stderr req))))

(defun neder--request-stderr-string (req)
  "Return REQ's accumulated stderr as one string."
  (apply #'concat (reverse (neder-request-stderr req))))

(defun neder--retrieve-response (req)
  "Return REQ's response text, or nil, recording any reported metadata.

Prefers the temp file and falls back to parsed stdout.  In unsafe mode
the temp file holds the answer verbatim while stdout is a transcript of
the CLI's actions, so the file branch is never parsed.  In safe mode
there is no temp file and the provider's `parse-response' turns stdout
into the answer.  The blank check matters on the file branch too,
because the file is pre-created empty."
  (let* ((file (neder-request-temp-file req))
         (from-file (and file
                         (file-readable-p file)
                         (neder--string-or-nil
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))))))
    (if from-file
        (neder--sanitize from-file)
      (let* ((stdout (neder--string-or-nil
                      (apply #'concat (reverse (neder-request-stdout req)))))
             (parser (and stdout
                          (neder-request-provider req)
                          (neder-provider-parse-response
                           (neder-request-provider req))))
             (parsed (and parser (funcall parser stdout))))
        (when parsed
          (setf (neder-request-cost req) (plist-get parsed :cost)
                (neder-request-session-id req) (plist-get parsed :session-id)
                (neder-request-turns req) (plist-get parsed :turns))
          (when-let* ((err (plist-get parsed :error)))
            (setf (neder-request-error req) (format "provider error: %s" err))))
        (neder--sanitize (if parsed (plist-get parsed :text) stdout))))))

(defun neder--release-markers (req)
  "Drop REQ's markers.  Separate so an async apply can hold them open."
  (dolist (slot '(start-marker end-marker))
    (let ((marker (cl-struct-slot-value 'neder-request slot req)))
      (when (markerp marker) (set-marker marker nil)))))

(defun neder--cleanup-request (req)
  "Release REQ's timers, overlay, markers and temp file.
Markers are kept when `pending-apply' is set, because an Ediff session
applying the answer later still needs them."
  (when-let* ((timer (neder-request-timer req))) (cancel-timer timer))
  (when-let* ((timer (neder-request-kill-timer req))) (cancel-timer timer))
  (setf (neder-request-timer req) nil
        (neder-request-kill-timer req) nil)
  (neder--spinner-stop req)
  (when-let* ((file (neder-request-temp-file req)))
    (when (file-exists-p file)
      (ignore-errors (delete-file file))))
  (unless (neder-request-pending-apply req)
    (neder--release-markers req))
  (setf (neder-request-process req) nil))

(defun neder--process-sentinel (req)
  "Return a process sentinel closing over REQ."
  (lambda (proc _event)
    (unless (or (process-live-p proc) (neder-request-settled req))
      (setf (neder-request-settled req) t)
      (unwind-protect
          (let ((status (process-exit-status proc)))
            (cond
             ;; Cancelled: do not touch the buffer, matching upstream's
             ;; is_cancelled check in its exit handler.
             ((eq (neder-request-state req) 'cancelled)
              (neder--log "request %d cancelled" (neder-request-id req))
              (message "neder[%d] cancelled" (neder-request-id req)))
             ((not (zerop status))
              (setf (neder-request-state req) 'failed
                    (neder-request-error req)
                    (format "process exit code: %d\n%s"
                            status (neder--request-stderr-string req)))
              (neder--log "request %d failed: %s"
                         (neder-request-id req) (neder-request-error req))
              (message "neder[%d] failed: %s" (neder-request-id req)
                       (string-trim
                        (or (neder--string-or-nil
                             (neder--request-stderr-string req))
                            (format "exit code %d" status)))))
             (t
              (let ((response (neder--retrieve-response req)))
                (cond
                 ;; The CLI can exit 0 and still report failure in the
                 ;; JSON envelope; `neder--retrieve-response' records it.
                 ((and (neder-request-error req)
                       (string-prefix-p "provider error:"
                                        (neder-request-error req)))
                  (setf (neder-request-state req) 'failed)
                  (message "neder[%d] %s" (neder-request-id req)
                           (neder-request-error req)))
                 ((null response)
                  (setf (neder-request-state req) 'failed
                        (neder-request-error req)
                        "unable to retrieve response")
                  (message "neder[%d] unable to retrieve response"
                           (neder-request-id req)))
                 (t
                  (setf (neder-request-state req) 'success)
                  (condition-case err
                      (funcall (neder-op-complete (neder--op (neder-request-op req)))
                               req response)
                    ;; An error here must not escape the sentinel.
                    (error
                     (setf (neder-request-state req) 'failed
                           (neder-request-error req) (error-message-string err))
                     (message "neder[%d] %s" (neder-request-id req)
                              (error-message-string err))))))))))
        (neder--cleanup-request req)))))

(defun neder--overlapping-request (buffer start end)
  "Return a live request tracking a region of BUFFER overlapping START..END."
  (seq-find
   (lambda (r)
     (and (eq (neder-request-state r) 'requesting)
          (eq (neder-request-buffer r) buffer)
          (markerp (neder-request-start-marker r))
          (marker-position (neder-request-start-marker r))
          (markerp (neder-request-end-marker r))
          (marker-position (neder-request-end-marker r))
          (< start (marker-position (neder-request-end-marker r)))
          (> end (marker-position (neder-request-start-marker r)))))
   neder--requests))

(defun neder--start-request (req)
  "Spawn the provider process for REQ."
  (let* ((provider (neder-request-provider req))
         (exe (neder--executable provider)))
    (unless exe
      (user-error (concat "neder: cannot find `%s'.  Emacs exec-path does not "
                          "include it, which is usual when Emacs was started "
                          "from a desktop launcher rather than a shell.  "
                          "Either add its directory to `exec-path' or set "
                          "`neder-executable-overrides' to an absolute path")
                  (neder-provider-executable provider)))
    ;; A buffer visiting a since-deleted directory would otherwise make
    ;; `make-process' signal a raw file-missing from inside the spawn.
    (unless (file-directory-p (neder-request-directory req))
      (user-error "neder: working directory does not exist: %s"
                  (neder-request-directory req)))
    (when (>= (length (neder--active-requests)) neder-max-concurrent-requests)
      (user-error "neder: too many in-flight requests (%d); stop some with `neder-stop-all-requests'"
                  neder-max-concurrent-requests))
    (let* ((argv (let ((command (neder--build-command
                                 provider (neder-request-prompt-string req)
                                 (neder-request-model req))))
                   ;; Run the resolved program, not the bare name, so an
                   ;; absolute override works regardless of exec-path.
                   (cons exe (cdr command))))
           (name (format "neder-%d" (neder-request-id req)))
           ;; This is how Emacs sets the child's working directory.
           (default-directory (neder-request-directory req))
           (stderr (make-pipe-process :name (concat name "-stderr")
                                      :buffer nil
                                      :noquery t
                                      :filter (neder--stderr-filter req)))
           (proc (make-process
                  :name name
                  ;; No process buffer: the filter owns the data.
                  :buffer nil
                  ;; A list command execs directly, with no shell.  The
                  ;; prompt is multi-kilobyte text full of newlines and
                  ;; angle brackets, so a shell would be both a quoting
                  ;; and an injection hazard.
                  :command argv
                  :connection-type 'pipe
                  :noquery t
                  :filter (neder--process-filter req)
                  :stderr stderr
                  :sentinel (neder--process-sentinel req))))
      (setf (neder-request-process req) proc
            (neder-request-state req) 'requesting)
      (neder--spinner-start req)
      (when neder-request-timeout
        (setf (neder-request-timer req)
              (run-at-time neder-request-timeout nil
                           (lambda ()
                             (when (eq (neder-request-state req) 'requesting)
                               (setf (neder-request-error req)
                                     (format "timed out after %s seconds"
                                             neder-request-timeout))
                               (neder--cancel-request req)
                               (message "neder[%d] timed out after %s seconds"
                                        (neder-request-id req)
                                        neder-request-timeout))))))
      (neder--log "request %d started: %S" (neder-request-id req) argv)
      (message "neder[%d] %s running..." (neder-request-id req)
               (neder-provider-display-name provider))
      req)))

(defun neder--new-request (op user-prompt)
  "Create and register a request for OP with USER-PROMPT."
  (let ((req (neder--request-create
              :id (cl-incf neder--request-counter)
              :op op
              :provider (neder--provider)
              :model (or neder-model
                         (neder-provider-default-model (neder--provider)))
              :buffer (current-buffer)
              :directory (neder--project-root)
              :user-prompt user-prompt
              :temp-file (when neder-unsafe-skip-permissions
                           (neder--make-temp-file)))))
    (push req neder--requests)
    req))

(defun neder--cancel-request (req)
  "Cancel REQ.  Idempotent, like upstream's `Prompt:cancel'."
  (when (eq (neder-request-state req) 'requesting)
    ;; Set the state before signalling so a racing filter sees it.
    (setf (neder-request-state req) 'cancelled)
    (neder--spinner-stop req)
    (when-let* ((proc (neder-request-process req)))
      (when (process-live-p proc)
        (ignore-errors (signal-process proc 'SIGTERM))
        ;; Upstream stops at SIGTERM; a CLI that ignores it would pin the
        ;; request in the registry forever.
        (setf (neder-request-kill-timer req)
              (run-at-time neder-kill-grace-seconds nil
                           (lambda ()
                             (when (process-live-p proc)
                               (neder--log "escalating to SIGKILL for %d"
                                          (neder-request-id req))
                               (ignore-errors
                                 (signal-process proc 'SIGKILL))))))))
    t))


;;;; Prompt entry

(defvar neder--prompt-result nil
  "Prompt text captured by `neder-prompt-send', consumed by `neder--read-prompt'.")

(defvar-keymap neder-prompt-mode-map
  :doc "Keymap for `neder-prompt-mode'."
  "C-c C-c" #'neder-prompt-send
  "C-c C-k" #'neder-prompt-abort)

(define-derived-mode neder-prompt-mode text-mode "Neder-Prompt"
  "Major mode for composing a Neder prompt.

\\<neder-prompt-mode-map>Send with \\[neder-prompt-send], abort with \
\\[neder-prompt-abort].

Type # to complete a skill name and @ to complete a project file; both
are resolved into the prompt when the request is assembled."
  (setq-local completion-at-point-functions
              (list #'neder-capf-skill #'neder-capf-file))
  (setq-local header-line-format
              (substitute-command-keys
               "Neder prompt -- \\[neder-prompt-send] send, \\[neder-prompt-abort] abort, # skill, @ file")))

(defun neder-prompt-send ()
  "Finish editing the prompt and run the request."
  (interactive)
  (setq neder--prompt-result
        (string-trim (buffer-substring-no-properties (point-min) (point-max))))
  (exit-recursive-edit))

(defun neder-prompt-abort ()
  "Abandon the prompt."
  (interactive)
  (setq neder--prompt-result nil)
  (abort-recursive-edit))

(defun neder-capf-skill ()
  "Completion-at-point function for #skill references."
  (save-excursion
    (let ((end (point)))
      (when (looking-back "#\\([[:alnum:]_./-]*\\)" (line-beginning-position))
        (list (match-beginning 1) end
              (mapcar #'car (neder--skills))
              :annotation-function (lambda (_) " skill")
              :exclusive 'no)))))

(defun neder-capf-file ()
  "Completion-at-point function for @file references."
  (save-excursion
    (let ((end (point)))
      (when (looking-back "@\\([^[:space:]]*\\)" (line-beginning-position))
        (list (match-beginning 1) end
              (neder--project-files)
              :annotation-function (lambda (_) " file")
              :exclusive 'no)))))

(defun neder--read-prompt (title)
  "Read a prompt labelled TITLE, in a buffer or the minibuffer.

The buffer path runs a `recursive-edit' so the caller still gets the
prompt as a return value.  That keeps every op synchronous, which is
what lets the interactive specs stay one line long."
  (if (not neder-use-prompt-buffer)
      (read-string (concat title ": "))
    (let ((buf (get-buffer-create "*neder-prompt*"))
          (win nil))
      (setq neder--prompt-result nil)
      (with-current-buffer buf
        (erase-buffer)
        (neder-prompt-mode)
        (setq-local header-line-format
                    (substitute-command-keys
                     (format "Neder %s -- \\[neder-prompt-send] send, \\[neder-prompt-abort] abort, # skill, @ file"
                             title))))
      (setq win (display-buffer buf '((display-buffer-below-selected)
                                      (window-height . 8))))
      (select-window win)
      (unwind-protect (recursive-edit)
        (when (window-live-p win) (quit-window nil win)))
      (or (neder--string-or-nil neder--prompt-result)
          (user-error "neder: you must add a prompt to your request")))))


;;;; Region op scaffolding

(defun neder--visual-bounds ()
  "Return (START . END) for the active region.
Under `evil-visual-line' the bounds snap to whole lines, which is what
the operation means: upstream's range comes from visual marks and
replaces whole lines."
  (let ((start (region-beginning))
        (end (region-end)))
    (when (and (boundp 'evil-state)
               (eq evil-state 'visual)
               (boundp 'evil-visual-selection)
               (eq evil-visual-selection 'line))
      (save-excursion
        (goto-char start)
        (setq start (line-beginning-position))
        (goto-char end)
        (setq end (line-end-position))))
    (cons start end)))

(defun neder--region-args (title)
  "Return the interactive argument list (START END PROMPT) for a region op.
TITLE labels the prompt."
  (if (use-region-p)
      (let ((bounds (neder--visual-bounds)))
        (list (car bounds) (cdr bounds) (neder--read-prompt title)))
    (user-error "neder: this command requires an active region")))

(defun neder--region-request (op-name start end user-prompt)
  "Create, populate and start a region request for OP-NAME.
START and END delimit the region; USER-PROMPT is the user's text."
  (let* ((op (neder--op op-name))
         (file (or (buffer-file-name) (buffer-name)))
         (selection (buffer-substring-no-properties start end))
         (location (save-excursion
                     (neder--region-location
                      file
                      (line-number-at-pos start)
                      (1+ (progn (goto-char start) (current-column)))
                      (line-number-at-pos end)
                      (1+ (progn (goto-char end) (current-column))))))
         (surrounding (car (neder--surrounding-context start end)))
         (extra (neder--extra-context start end))
         req)
    (when (and (neder-op-track-p op)
               (neder--overlapping-request (current-buffer) start end))
      (user-error "neder: another request is already tracking this region"))
    (setq req (neder--new-request op-name user-prompt))
    (setf (neder-request-selection req) selection
          (neder-request-location req) location
          (neder-request-surrounding req) surrounding
          (neder-request-extra-context req) extra
          (neder-request-major-mode-name req) major-mode)
    (when (neder-op-track-p op)
      ;; Markers are the extmark analogue.  The end marker takes insertion
      ;; type t so text typed at the tail during the request stays inside
      ;; the tracked region, mirroring extmark end-gravity.  Because markers
      ;; track edits, the replacement lands correctly even if the buffer
      ;; shifts mid-request.
      (setf (neder-request-start-marker req) (copy-marker start nil)
            (neder-request-end-marker req) (copy-marker end t)
            (neder-request-original-length req) (- end start)
            (neder-request-original-text req) selection))
    (neder--push-context
     req (neder--op-prompt (funcall (neder-op-op-prompt op) req) user-prompt))
    (if (neder-op-track-p op)
        (neder--finalize-prompt
         req
         (format "<Location><File>%s</File><Function>%s</Function></Location>"
                 file location)
         (format "<FunctionText>%s</FunctionText>" selection))
      (neder--finalize-prompt req))
    (neder--start-request req)))

;;;###autoload
(cl-defun neder-run (&key op prompt start end)
  "Run the registered operation OP with PROMPT.

START and END are required for region ops and ignored otherwise.  This
is the entry point for ops defined in user config, so a new workflow is
a keybinding rather than a patch:

  (defun my/neder-debug-tests ()
    (interactive)
    (neder-run :op \\='search
              :prompt \"run `make test' and locate each failure\"))"
  (let ((entry (neder--op op)))
    (if (neder-op-region-p entry)
        (progn
          (unless (and start end)
            (user-error "neder: op `%s' requires a region" op))
          (neder--region-request op start end prompt))
      (let ((req (neder--new-request op prompt)))
        (neder--push-context
         req (neder--op-prompt (funcall (neder-op-op-prompt entry) req) prompt))
        (neder--finalize-prompt req)
        (neder--start-request req)))))


;;;; Op: visual

(defun neder--visual-selection-changed-p (req start end)
  "Return non-nil when REQ's tracked text at START..END is not what it was."
  (let ((original (neder-request-original-text req)))
    (and original
         (not (string= original
                       (buffer-substring-no-properties start end))))))

(defun neder--visual-text (req start end response)
  "Return the text to insert for RESPONSE over REQ's region START..END."
  (let* ((original (buffer-substring-no-properties start end))
         ;; Whitespace between the start of the line and the selection.
         ;; When that is all there is, the selection began at the line's
         ;; indentation and the replacement should line up with it.
         (prefix (save-excursion
                   (goto-char start)
                   (buffer-substring-no-properties
                    (line-beginning-position) (point))))
         (indent (if (string-blank-p prefix)
                     prefix
                   (save-excursion
                     (goto-char start)
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (progn (back-to-indentation) (point))))))
         (text (if neder-visual-indent
                   (neder--reindent response indent)
                 response)))
    (ignore req)
    ;; The line-array API upstream made this implicit; with string
    ;; insertion it has to be explicit.
    (cond
     ((and (string-suffix-p "\n" original) (not (string-suffix-p "\n" text)))
      (concat text "\n"))
     ((and (not (string-suffix-p "\n" original)) (string-suffix-p "\n" text))
      (string-trim-right text "\n+"))
     (t text))))

(defun neder--apply-replacement (req start end text)
  "Replace START..END with TEXT in REQ's buffer as one undo step."
  (save-excursion
    (undo-boundary)
    ;; atomic-change-group makes this one undo unit and rolls back
    ;; if the insert errors, so a half-applied replacement is
    ;; impossible.  Upstream has no undo handling at all.
    (atomic-change-group
      (delete-region start end)
      (goto-char start)
      (insert text))
    (undo-boundary))
  (message "neder[%d] applied %d characters"
           (neder-request-id req) (length text)))

(defun neder--diff-string (old new)
  "Return a unified diff between strings OLD and NEW."
  (let ((a (make-temp-file "neder-old-"))
        (b (make-temp-file "neder-new-")))
    (unwind-protect
        (progn
          (write-region old nil a nil 'silent)
          (write-region new nil b nil 'silent)
          (with-temp-buffer
            (call-process "diff" nil t nil "-u"
                          "--label" "current" "--label" "proposed" a b)
            (buffer-string)))
      (ignore-errors (delete-file a))
      (ignore-errors (delete-file b)))))

(defun neder--confirm-replacement (req old new)
  "Show a diff between OLD and NEW for REQ and ask whether to apply it."
  (let ((buf (get-buffer-create "*neder-diff*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (neder--diff-string old new)))
      (goto-char (point-min))
      (diff-mode)
      (set-buffer-modified-p nil))
    (save-window-excursion
      (display-buffer buf)
      (unwind-protect
          (y-or-n-p (format "neder[%d]: apply this replacement? "
                            (neder-request-id req)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(defun neder--ediff-replacement (req start end text)
  "Open an Ediff session for REQ's region, applying the B buffer on quit.

The request is marked `pending-apply' so `neder--cleanup-request' leaves
the markers alive: the sentinel has long returned by the time Ediff
ends, and without the markers there would be nothing left pointing at
the region."
  (require 'ediff)
  (let* ((id (neder-request-id req))
         (buffer (neder-request-buffer req))
         (a (generate-new-buffer (format " *neder-%d-current*" id)))
         (b (generate-new-buffer (format "*neder-%d-proposed*" id)))
         (mode (neder-request-major-mode-name req)))
    (setf (neder-request-pending-apply req) t)
    (with-current-buffer a
      (insert (buffer-substring-no-properties start end))
      (when (fboundp mode) (funcall mode)))
    (with-current-buffer b
      (insert text)
      (when (fboundp mode) (funcall mode)))
    (ediff-buffers
     a b
     (list (lambda ()
             (add-hook 'ediff-quit-hook
                       (lambda ()
                         (let ((new (and (buffer-live-p b)
                                         (with-current-buffer b
                                           (buffer-string)))))
                           (unwind-protect
                               (when (and new
                                          (buffer-live-p buffer)
                                          (markerp (neder-request-start-marker req))
                                          (marker-position
                                           (neder-request-start-marker req)))
                                 (with-current-buffer buffer
                                   (neder--apply-replacement
                                    req
                                    (neder-request-start-marker req)
                                    (neder-request-end-marker req)
                                    new)))
                             (setf (neder-request-pending-apply req) nil)
                             (neder--release-markers req)
                             (when (buffer-live-p a) (kill-buffer a))
                             (when (buffer-live-p b) (kill-buffer b)))))
                       nil t))))))

(defun neder--visual-complete (req response)
  "Replace REQ's tracked region with RESPONSE, honouring the apply style."
  (let* ((start (neder-request-start-marker req))
         (end (neder-request-end-marker req))
         (buffer (and (markerp start) (marker-buffer start))))
    (unless (and buffer
                 (buffer-live-p buffer)
                 (marker-buffer end)
                 (<= (marker-position start) (marker-position end))
                 ;; Deleting the selected text collapses both markers onto
                 ;; the same position rather than invalidating them, so a
                 ;; span that was non-empty and is now empty means the user
                 ;; deleted it mid-request.  Without this check the answer
                 ;; would be inserted into unrelated surroundings.
                 (not (and (> (or (neder-request-original-length req) 0) 0)
                           (= (marker-position start) (marker-position end)))))
      (error "neder: the original selection was destroyed during the request"))
    ;; The markers carry their own buffer, but the request records which
    ;; buffer asked for the work.  Asserting they agree is what makes
    ;; "always the same buffer that generated the request" an invariant
    ;; rather than an observation.
    (unless (eq buffer (neder-request-buffer req))
      (error "neder: marker buffer is not the buffer that made the request"))
    (with-current-buffer buffer
      (when buffer-read-only
        (error "neder: %s is read-only" (buffer-name buffer)))
      (when (and neder-confirm-on-changed-selection
                 (neder--visual-selection-changed-p req start end))
        (unless (y-or-n-p
                 (format "neder[%d]: the selection changed during the request.  \
The answer was computed from the old text.  Replace anyway? "
                         (neder-request-id req)))
          (error "neder: aborted, the selection changed during the request")))
      (let ((text (neder--visual-text req start end response)))
        (pcase neder-visual-apply-style
          ('direct (neder--apply-replacement req start end text))
          ('ediff (neder--ediff-replacement req start end text))
          (_
           (if (neder--confirm-replacement
                req (buffer-substring-no-properties start end) text)
               (neder--apply-replacement req start end text)
             (message "neder[%d] replacement declined" (neder-request-id req)))))))))

(neder-register-op
 (neder-op-create
  :name 'visual
  :region-p t
  :track-p t
  :op-prompt
  (lambda (req)
    (format neder--prompt-visual
            (neder--org-src-lang (neder-request-major-mode-name req))
            (neder-request-location req)
            (neder-request-selection req)
            (neder-request-surrounding req)))
  :complete #'neder--visual-complete))

;;;###autoload
(defun neder-visual (start end prompt)
  "Replace the region between START and END with the model's answer to PROMPT."
  (interactive (neder--region-args "visual prompt"))
  (neder--region-request 'visual start end prompt))


;;;; Op: ask

(defun neder--org-src-lang (mode)
  "Return an Org src-block language for MODE."
  (if (null mode)
      ""
    (let ((name (string-remove-suffix "-mode" (symbol-name mode))))
      ;; Org expects the base language name; -ts- modes carry a suffix.
      (string-remove-suffix "-ts" name))))

(defun neder--ask-complete (req response)
  "Render REQ's RESPONSE as Org in the answer buffer.
The source buffer is never touched."
  (neder--rename-stale-buffer "*neder-answer*")
  (with-current-buffer (get-buffer-create "*neder-answer*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "#+title: neder answer\n\n"
              "* Question\n"
              (neder-request-user-prompt req) "\n\n"
              "* Selection\n"
              (format "Location: =%s=\n\n" (or (neder-request-location req) "?"))
              (format "#+begin_src %s\n%s%s#+end_src\n\n"
                      (neder--org-src-lang (neder-request-major-mode-name req))
                      (or (neder-request-selection req) "")
                      (if (string-suffix-p "\n" (or (neder-request-selection req) ""))
                          "" "\n"))
              "* Answer\n"
              response
              (if (string-suffix-p "\n" response) "" "\n")))
    (goto-char (point-min))
    (when (fboundp 'org-mode) (org-mode))
    (set-buffer-modified-p nil)
    (setq-local neder--buffer-request-id (neder-request-id req))
    (setf (neder-request-result-buffer req) (current-buffer)))
  (pop-to-buffer "*neder-answer*")
  (message "neder[%d] answered%s" (neder-request-id req)
           (if (neder-request-cost req)
               (format " ($%.4f)" (neder-request-cost req)) "")))

(neder-register-op
 (neder-op-create
  :name 'ask
  :region-p t
  :track-p nil
  :op-prompt
  (lambda (req)
    (format neder--prompt-ask
            (neder-request-location req)
            (neder-request-selection req)
            (neder-request-surrounding req)))
  :complete #'neder--ask-complete))

;;;###autoload
(defun neder-ask (start end question)
  "Answer QUESTION about the region between START and END.

Unlike `neder-visual', this never modifies the source buffer.  The answer
is rendered as Org in the =*neder-answer*= buffer."
  (interactive (neder--region-args "ask"))
  (neder--region-request 'ask start end question))


;;;; Op: search

(defun neder--parse-search-line (line)
  "Parse LINE as PATH:LNUM:CNUM,X,NOTES.
Return a plist of :file :line :column :span :notes, or nil.
Port of `M.parse_line' in lua/99/ops/qfix-helpers.lua."
  (when (string-match "\\`\\(.+?\\):\\([^:]+\\):\\(.+\\)\\'" line)
    (let ((file (match-string 1 line))
          (lnum (match-string 2 line))
          (rest (match-string 3 line)))
      (when (string-match "\\`\\([^,]+\\),\\([^,]+\\),?\\(.*\\)\\'" rest)
        (let ((col (match-string 1 rest))
              (span (match-string 2 rest))
              (notes (match-string 3 rest)))
          (list :file file
                ;; Upstream falls back to 1 when the number does not parse.
                :line (let ((n (string-to-number lnum))) (if (> n 0) n 1))
                :column (let ((n (string-to-number col))) (if (> n 0) n 1))
                ;; Upstream discards the span; we keep it for display.
                :span (string-trim span)
                :notes (or notes "")))))))

(defun neder--parse-search-response (response)
  "Return the parsed hits in RESPONSE, skipping unparseable lines."
  (seq-keep #'neder--parse-search-line (split-string response "\n")))

(defun neder--search-complete (req response)
  "Render REQ's RESPONSE into a `grep-mode' buffer."
  (let ((hits (neder--parse-search-response response))
        (root (neder-request-directory req)))
    (if (null hits)
        (message "neder: no search results")
      (neder--rename-stale-buffer "*neder-search*")
      (with-current-buffer (get-buffer-create "*neder-search*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "neder search: " (neder-request-user-prompt req) "\n\n")
          (dolist (hit hits)
            ;; grep's regexps want a colon after the column, not a comma,
            ;; so reshape PATH:LNUM:CNUM,X,NOTES into PATH:LNUM:CNUM:NOTES
            ;; and keep the span in the text so nothing is lost.
            (insert (format "%s:%d:%d:[+%s] %s\n"
                            (plist-get hit :file)
                            (plist-get hit :line)
                            (plist-get hit :column)
                            (plist-get hit :span)
                            (plist-get hit :notes))))
          (insert (format "\nneder search finished, %d result%s\n"
                          (length hits) (if (= 1 (length hits)) "" "s"))))
        (grep-mode)
        (setq-local default-directory root)
        (setq-local neder--buffer-request-id (neder-request-id req))
        (setf (neder-request-result-buffer req) (current-buffer))
        (goto-char (point-min)))
      (pop-to-buffer "*neder-search*")
      (message "neder[%d] %d result%s" (neder-request-id req)
               (length hits) (if (= 1 (length hits)) "" "s")))))

(neder-register-op
 (neder-op-create
  :name 'search
  :region-p nil
  :track-p nil
  :op-prompt (lambda (_req) neder--prompt-search)
  :complete #'neder--search-complete))

;;;###autoload
(defun neder-search (prompt)
  "Run an agentic project search for PROMPT into a `grep-mode' buffer."
  (interactive (list (neder--read-prompt "search")))
  (neder-run :op 'search :prompt prompt))


;;;; Commands

;;;###autoload
(defun neder-stop-all-requests ()
  "Cancel every in-flight request.  Safe to call when there are none."
  (interactive)
  (let ((n 0))
    (dolist (req (neder--active-requests))
      (when (neder--cancel-request req) (cl-incf n)))
    (when (called-interactively-p 'interactive)
      (message (if (zerop n)
                   "neder: no requests in flight"
                 (format "neder: cancelled %d request%s" n
                         (if (= n 1) "" "s")))))
    n))

;;;###autoload
(defun neder-stop-request ()
  "Cancel the most recent in-flight request."
  (interactive)
  (if-let* ((req (car (neder--active-requests))))
      (progn (neder--cancel-request req)
             (message "neder: cancelled request %d" (neder-request-id req)))
    (message "neder: no requests in flight")))

(defun neder--request-label (req)
  "Return a one-line label describing REQ for completion."
  (format "%-4d %-7s %-9s %s"
          (neder-request-id req)
          (neder-request-op req)
          (neder-request-state req)
          (string-replace "\n" " "
                          (truncate-string-to-width
                           (or (neder-request-user-prompt req) "") 60))))

;;;###autoload
(defun neder-open ()
  "Pick a past request and show its output again.
Port of upstream's `open', which reopens the last interaction in
whatever display makes sense for its type."
  (interactive)
  (let* ((candidates
          (seq-keep (lambda (r)
                      (when (buffer-live-p (neder-request-result-buffer r))
                        (cons (neder--request-label r) r)))
                    neder--requests)))
    (if (null candidates)
        (message "neder: no results to reopen")
      (let* ((choice (completing-read "neder result: " candidates nil t))
             (req (cdr (assoc choice candidates))))
        (pop-to-buffer (neder-request-result-buffer req))))))

;;;###autoload
(defun neder-info ()
  "Report request counts, the active provider and the session's cost."
  (interactive)
  (let ((cost (seq-reduce #'+ (seq-keep #'neder-request-cost neder--requests) 0)))
    (message "neder: %d in flight, %d completed, provider %s%s"
             (length (neder--active-requests))
             (seq-count (lambda (r) (eq (neder-request-state r) 'success))
                        neder--requests)
             (neder-provider-display-name (neder--provider))
             (if (> cost 0) (format ", $%.4f this session" cost) ""))))

(add-hook 'kill-emacs-hook #'neder-stop-all-requests)

(provide 'neder)
;;; neder.el ends here
