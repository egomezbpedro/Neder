;;; neder-tests.el --- Tests for neder.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs -Q --batch -L . -l neder-tests.el -f ert-run-tests-batch-and-exit
;;
;; Upstream ships a controllable test provider so the request lifecycle can
;; be exercised without a real CLI (see AGENTS.md and 99.test.test_utils).
;; `neder-test--with-stub-provider' is the analogue: it registers a provider
;; whose argv is a plain shell stub, so no model is ever called.

;;; Code:

(require 'ert)
(require 'neder)

(defun neder-test--register-stub (argv-fn)
  "Register a stub provider whose build-command is ARGV-FN."
  (neder-register-provider
   (neder-provider-create
    :name 'stub
    :display-name "StubProvider"
    :executable "sh"
    :default-model "stub-model"
    :build-command argv-fn)))

(defmacro neder-test--with-stub (argv-fn &rest body)
  "Run BODY with the stub provider built by ARGV-FN selected.

The apply style is pinned to `direct' and the spinner is off: the
default `confirm' style would block on `y-or-n-p' in batch, and the
spinner would leave timers running past the end of the test.  Tests
that exercise those two features bind them back on themselves."
  (declare (indent 1))
  `(let ((neder-provider 'stub)
         (neder-model nil)
         (neder-requests-before neder--requests)
         (neder-request-timeout 10)
         (neder-unsafe-skip-permissions nil)
         (neder-visual-apply-style 'direct)
         (neder-spinner nil)
         (neder-context-strategy 'lines)
         (neder-include-diagnostics nil)
         (neder-include-hover nil)
         (neder-use-prompt-buffer nil)
         (neder-debug nil))
     (neder-test--register-stub ,argv-fn)
     (unwind-protect (progn ,@body)
       (neder-stop-all-requests)
       (setq neder--requests neder-requests-before))))

(defun neder-test--wait (req &optional seconds)
  "Block until REQ leaves the `requesting' state, or SECONDS elapse."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (eq (neder-request-state req) 'requesting)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (neder-request-state req)))


;;;; Search line parsing

(ert-deftest neder-test-parse-search-line ()
  (let ((hit (neder--parse-search-line "/a/b.js:24:8,3,notes, with commas")))
    (should (equal (plist-get hit :file) "/a/b.js"))
    (should (equal (plist-get hit :line) 24))
    (should (equal (plist-get hit :column) 8))
    (should (equal (plist-get hit :span) "3"))
    ;; The comma-containing notes case is what upstream's regexp is shaped for.
    (should (equal (plist-get hit :notes) "notes, with commas"))))

(ert-deftest neder-test-parse-search-line-rejects-garbage ()
  (should-not (neder--parse-search-line "this is prose, not a location"))
  (should-not (neder--parse-search-line ""))
  (should-not (neder--parse-search-line "/a/b.js:24:no-commas-here")))

(ert-deftest neder-test-parse-search-line-defaults-to-one ()
  ;; Upstream falls back to 1 when the number does not parse.
  (let ((hit (neder--parse-search-line "/a/b.js:x:y,1,notes")))
    (should (equal (plist-get hit :line) 1))
    (should (equal (plist-get hit :column) 1))))

(ert-deftest neder-test-parse-search-response-skips-bad-lines ()
  (let ((hits (neder--parse-search-response
               "preamble the model should not have written\n/a/b.js:1:1,1,one\n\n/c/d.js:2:2,2,two\n")))
    (should (= 2 (length hits)))
    (should (equal "/a/b.js" (plist-get (car hits) :file)))))


;;;; Response sanitizing

(ert-deftest neder-test-unfence ()
  (should (equal (neder--sanitize "```elisp\n(+ 1 2)\n```") "(+ 1 2)"))
  (should (equal (neder--sanitize "```\nplain\n```") "plain"))
  ;; Two separate blocks must survive untouched.
  (should (string-match-p "```" (neder--sanitize "```\na\n```\ntext\n```\nb\n```"))))

(ert-deftest neder-test-sanitize-blank-is-nil ()
  (should-not (neder--sanitize "   \n\t\n"))
  (should-not (neder--sanitize nil)))

(ert-deftest neder-test-sanitize-strips-ansi ()
  (should (equal (neder--sanitize "\e[31mred\e[0m") "red")))


;;;; Reindentation

(ert-deftest neder-test-dedent ()
  (should (equal (neder--dedent "   a\n   b") "a\nb"))
  (should (equal (neder--dedent "  a\n    b") "a\n  b"))
  ;; Blank lines must not drag the common prefix to zero.
  (should (equal (neder--dedent "    a\n\n    b") "a\n\nb"))
  (should (equal (neder--dedent "no indent") "no indent")))

(ert-deftest neder-test-reindent-applies-selection-indent ()
  ;; The first line is bare: it is inserted at a point already sitting at
  ;; the selection's indentation.
  (should (equal (neder--reindent "   a\n   b" "    ") "a\n    b"))
  (should (equal (neder--reindent "a\nb" "") "a\nb"))
  ;; Blank lines stay blank rather than becoming trailing whitespace.
  (should (equal (neder--reindent "   a\n\n   b" "  ") "a\n\n  b")))

(ert-deftest neder-test-reindent-normalizes-model-indentation ()
  "A CLI emitting 3-space indentation must land on the buffer's 4."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "printf '   x = 1\\n   return x'"))
    (with-temp-buffer
      (insert "def f():\n    # TODO\n")
      (let* ((start (progn (goto-char (point-min))
                           (forward-line 1)
                           (back-to-indentation)
                           (point)))
             (end (line-end-position))
             (req (neder-visual start end "implement")))
        (should (eq 'success (neder-test--wait req)))
        (should (equal "def f():\n    x = 1\n    return x\n"
                       (buffer-string)))))))


;;;; Context walk

(ert-deftest neder-test-context-walk-collects-every-level ()
  "Both the root and the nested file must be collected.
This is the test that catches a `locate-dominating-file' regression,
since that function returns only the nearest hit."
  (let* ((root (file-name-as-directory (make-temp-file "neder-ctx" t)))
         (sub (file-name-as-directory (expand-file-name "a/b" root))))
    (unwind-protect
        (progn
          (make-directory sub t)
          (write-region "ROOT RULES" nil (expand-file-name "CLAUDE.md" root)
                        nil 'silent)
          (write-region "LEAF RULES" nil (expand-file-name "CLAUDE.md" sub)
                        nil 'silent)
          (let ((found (neder--read-context-files sub root '("CLAUDE.md"))))
            (should (= 2 (length found)))
            (should (member "ROOT RULES" found))
            (should (member "LEAF RULES" found))))
      (delete-directory root t))))

(ert-deftest neder-test-context-walk-empty-by-default ()
  ;; Upstream's md_files defaults to {}, so nothing is read.
  (should-not (neder--read-context-files default-directory
                                        default-directory nil)))

(ert-deftest neder-test-context-walk-stops-at-root ()
  "A file above the root must not be collected."
  (let* ((outer (file-name-as-directory (make-temp-file "neder-ctx" t)))
         (root (file-name-as-directory (expand-file-name "root" outer))))
    (unwind-protect
        (progn
          (make-directory root t)
          (write-region "ABOVE" nil (expand-file-name "CLAUDE.md" outer)
                        nil 'silent)
          (should-not (neder--read-context-files root root '("CLAUDE.md"))))
      (delete-directory outer t))))


;;;; Prompt assembly

(ert-deftest neder-test-prompt-safe-mode-has-no-temp-file-tag ()
  (let* ((neder-unsafe-skip-permissions nil)
         (neder-context-file-names nil)
         (req (neder--request-create :id 0 :op 'search
                                    :directory default-directory
                                    :user-prompt "find it")))
    (neder--push-context req (neder--op-prompt "OP TEXT" "find it"))
    (let ((prompt (neder--finalize-prompt req)))
      (should-not (string-match-p "<TEMP_FILE>" prompt))
      (should (string-match-p "standard output only" prompt))
      ;; The upstream block forbids conversational output and would
      ;; suppress the stdout this mode depends on.
      (should-not (string-match-p "never provide the requested changes as conversational output"
                                  prompt))
      (should (string-match-p "<Context>\nOP TEXT\n</Context>" prompt))
      (should (string-match-p "<Prompt>\nfind it\n</Prompt>" prompt)))))

(ert-deftest neder-test-prompt-unsafe-mode-has-temp-file-tag ()
  (let* ((neder-unsafe-skip-permissions t)
         (neder-context-file-names nil)
         (neder-temp-directory (make-temp-file "neder-tmp" t))
         (req (neder--request-create :id 0 :op 'search
                                    :directory default-directory
                                    :user-prompt "find it"
                                    :temp-file (neder--make-temp-file))))
    (unwind-protect
        (progn
          (neder--push-context req (neder--op-prompt "OP TEXT" "find it"))
          (let ((prompt (neder--finalize-prompt req)))
            (should (string-match-p "<TEMP_FILE>" prompt))
            (should (string-match-p "NEVER alter any file other than TEMP_FILE"
                                    prompt))
            ;; Pre-created empty, so existence never implies content.
            (should (file-exists-p (neder-request-temp-file req)))
            (should-not (neder--retrieve-response req))))
      (delete-directory neder-temp-directory t))))

(ert-deftest neder-test-empty-user-prompt-errors ()
  (should-error (neder--op-prompt "OP" "") :type 'user-error)
  (should-error (neder--op-prompt "OP" "   ") :type 'user-error))


;;;; Provider argv

(ert-deftest neder-test-claude-argv-omits-flag-by-default ()
  (let ((neder-provider 'claude)
        (neder-model "test-model")
        (neder-provider-extra-args nil)
        (neder-unsafe-skip-permissions nil))
    (let ((argv (neder--build-command (neder--provider) "PROMPT")))
      (should-not (member "--dangerously-skip-permissions" argv))
      ;; The prompt is always the last positional element.
      (should (equal (car (last argv)) "PROMPT"))
      ;; The JSON envelope is what makes safe mode trustworthy: the
      ;; answer is a named field rather than prose to be guessed at.
      (should (equal argv '("claude" "--model" "test-model"
                            "--output-format" "json"
                            "--print" "PROMPT"))))))

(ert-deftest neder-test-claude-argv-includes-flag-when-unsafe ()
  (let ((neder-provider 'claude)
        (neder-model "test-model")
        (neder-unsafe-skip-permissions t))
    (should (member "--dangerously-skip-permissions"
                    (neder--build-command (neder--provider) "PROMPT")))))

(ert-deftest neder-test-extra-args-come-after-prompt ()
  (let ((neder-provider 'claude)
        (neder-model "m")
        (neder-provider-extra-args '("--max-turns" "0"))
        (neder-unsafe-skip-permissions nil))
    (should (equal (last (neder--build-command (neder--provider) "PROMPT") 3)
                   '("PROMPT" "--max-turns" "0")))))

(ert-deftest neder-test-opencode-argv ()
  (let ((neder-provider 'opencode)
        (neder-model nil)
        (neder-provider-extra-args nil))
    (should (equal (neder--build-command (neder--provider) "PROMPT")
                   '("opencode" "run" "--agent" "build" "-m"
                     "opencode/claude-sonnet-4-5" "PROMPT")))))


;;;; Op: ask

(ert-deftest neder-test-ask-does-not-modify-source-buffer ()
  "The whole point of `neder-ask': the source buffer must be untouched."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "printf '* Summary\\nIt adds two numbers.\\n'"))
    (ignore-errors (kill-buffer "*neder-answer*"))
    (with-temp-buffer
      (insert "def add(a, b):\n    return a + b\n")
      (let* ((original (buffer-string))
             (start (point-min))
             (end (1- (point-max)))
             (req (neder-ask start end "what does this do?")))
        (should (eq 'success (neder-test--wait req)))
        (should (equal original (buffer-string)))
        (should (get-buffer "*neder-answer*"))
        (with-current-buffer "*neder-answer*"
          (should (derived-mode-p 'org-mode))
          (let ((text (buffer-string)))
            (should (string-match-p "^\\* Question$" text))
            (should (string-match-p "what does this do\\?" text))
            (should (string-match-p "^\\* Answer$" text))
            (should (string-match-p "It adds two numbers\\." text))
            ;; The selection is embedded as an Org src block.
            (should (string-match-p "#\\+begin_src" text))
            (should (string-match-p "#\\+end_src" text))
            (should (string-match-p "return a \\+ b" text))))
        (kill-buffer "*neder-answer*")))))

(ert-deftest neder-test-ask-sets-no-markers ()
  "`neder-ask' applies nothing back, so it must not track markers."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe) (list "sh" "-c" "printf 'answer'"))
    (ignore-errors (kill-buffer "*neder-answer*"))
    (with-temp-buffer
      (insert "code\n")
      (let ((req (neder-ask (point-min) (1- (point-max)) "q")))
        (should-not (neder-request-start-marker req))
        (should-not (neder-request-end-marker req))
        (should (eq 'success (neder-test--wait req)))
        (should (equal "code\n" (buffer-string))))
      (ignore-errors (kill-buffer "*neder-answer*")))))

(ert-deftest neder-test-ask-prompt-demands-org ()
  (let* ((neder-unsafe-skip-permissions nil)
         (neder-context-file-names nil)
         (req (neder--request-create :id 0 :op 'ask
                                    :directory default-directory
                                    :user-prompt "what is this")))
    (neder--push-context
     req (neder--op-prompt (format neder--prompt-ask "loc" "sel" "ctx")
                          "what is this"))
    (let ((prompt (neder--finalize-prompt req)))
      (should (string-match-p "valid Org mode markup" prompt))
      (should (string-match-p "#\\+begin_src" prompt))
      ;; Answer headings must nest under the scaffold's level-one "* Answer".
      (should (string-match-p "level two" prompt))
      ;; It must not be told to rewrite the code.
      (should (string-match-p "Do not rewrite the code" prompt)))))

(ert-deftest neder-test-ask-requires-region ()
  (with-temp-buffer
    (insert "text\n")
    (deactivate-mark)
    (should-error (call-interactively #'neder-ask) :type 'user-error)))

(ert-deftest neder-test-org-src-lang ()
  (should (equal (neder--org-src-lang 'python-mode) "python"))
  (should (equal (neder--org-src-lang 'emacs-lisp-mode) "emacs-lisp"))
  ;; Tree-sitter modes carry a -ts- suffix Org does not know.
  (should (equal (neder--org-src-lang 'python-ts-mode) "python"))
  (should (equal (neder--org-src-lang nil) "")))


;;;; Executable resolution

(ert-deftest neder-test-executable-bare-name-uses-exec-path ()
  (let ((neder-provider 'claude)
        (neder-executable-overrides nil)
        (exec-path (list "/nonexistent-dir")))
    (should-not (neder--executable (neder--provider)))))

(ert-deftest neder-test-executable-absolute-override-bypasses-exec-path ()
  "An absolute override must work when Emacs did not inherit shell PATH."
  (let* ((tmp (make-temp-file "neder-exe"))
         (neder-provider 'claude)
         (exec-path (list "/nonexistent-dir")))
    (unwind-protect
        (progn
          (set-file-modes tmp #o755)
          (let ((neder-executable-overrides `((claude . ,tmp))))
            (should (equal tmp (neder--executable (neder--provider)))))
          ;; A non-executable or missing override resolves to nil rather
          ;; than being handed to make-process.
          (let ((neder-executable-overrides '((claude . "/nope/claude"))))
            (should-not (neder--executable (neder--provider)))))
      (delete-file tmp))))

(ert-deftest neder-test-executable-expands-tilde ()
  (let* ((neder-provider 'claude)
         (exec-path (list "/nonexistent-dir"))
         (neder-executable-overrides '((claude . "~/.local/bin/claude"))))
    (let ((resolved (neder--executable (neder--provider))))
      ;; Skip when this machine has no claude installed.
      (when resolved
        (should (file-name-absolute-p resolved))
        (should-not (string-prefix-p "~" resolved))))))

(ert-deftest neder-test-missing-directory-errors-before-spawn ()
  "A deleted working directory must be a clean error, not a raw file-missing."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe) (list "sh" "-c" "printf 'x'"))
    (let ((dir (file-name-as-directory (make-temp-file "neder-gone" t))))
      (delete-directory dir t)
      (with-temp-buffer
        (insert "text\n")
        (setq-local default-directory dir)
        (let ((err (should-error (neder-visual (point-min) (1- (point-max)) "x")
                                 :type 'user-error)))
          (should (string-match-p "working directory does not exist"
                                  (error-message-string err))))))))

(ert-deftest neder-test-missing-executable-errors-before-spawn ()
  (let ((neder-provider 'claude)
        (neder-executable-overrides '((claude . "/nope/claude")))
        (exec-path (list "/nonexistent-dir")))
    (with-temp-buffer
      (insert "text\n")
      (should-error (neder-visual (point-min) (1- (point-max)) "x")
                    :type 'user-error))))


;;;; Request lifecycle

(ert-deftest neder-test-stdout-fallback-applies-replacement ()
  "The stdout fallback is what makes safe mode work at all."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "printf 'REPLACED'"))
    (with-temp-buffer
      (insert "line one\nTODO implement\nline three\n")
      (let* ((start (progn (goto-char (point-min))
                           (forward-line 1) (point)))
             (end (line-end-position))
             req)
        (set-mark start)
        (goto-char end)
        (activate-mark)
        (let ((neder-visual-indent nil))
          (setq req (neder-visual start end "implement this")))
        (should (eq 'success (neder-test--wait req)))
        (should (string-match-p "REPLACED" (buffer-string)))
        (should-not (string-match-p "TODO implement" (buffer-string)))))))

(ert-deftest neder-test-undo-reverts-in-one-step ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "printf 'NEW TEXT'"))
    (with-temp-buffer
      (buffer-enable-undo)
      (insert "alpha\nbeta\ngamma\n")
      (undo-boundary)
      (let* ((start (progn (goto-char (point-min))
                           (forward-line 1) (point)))
             (end (line-end-position))
             (original (buffer-string))
             (neder-visual-indent nil)
             (req (neder-visual start end "rewrite")))
        (should (eq 'success (neder-test--wait req)))
        (should (string-match-p "NEW TEXT" (buffer-string)))
        (undo-boundary)
        (undo)
        (should (equal original (buffer-string)))))))

(ert-deftest neder-test-marker-tracks-buffer-shift ()
  "The replacement must land on the shifted region, not the old offsets."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "sleep 0.4; printf 'LANDED'"))
    (with-temp-buffer
      (insert "a\nb\nTARGET\nc\n")
      (let* ((start (progn (goto-char (point-min))
                           (forward-line 2) (point)))
             (end (line-end-position))
             (neder-visual-indent nil)
             (req (neder-visual start end "replace")))
        ;; Insert text above the region while the request is in flight.
        (goto-char (point-min))
        (insert "inserted\nlines\nabove\n")
        (should (eq 'success (neder-test--wait req)))
        (should (string-match-p "^LANDED$" (buffer-string)))
        (should-not (string-match-p "TARGET" (buffer-string)))
        ;; The text inserted during the request must survive.
        (should (string-match-p "inserted" (buffer-string)))))))

(ert-deftest neder-test-lands-in-origin-buffer-not-current ()
  "Switching buffers mid-request must not redirect the replacement.
The markers carry their own buffer, so the edit lands where the
selection was made."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "sleep 0.4; printf 'LANDED'"))
    (let ((origin (generate-new-buffer "neder-origin"))
          (other (generate-new-buffer "neder-other")))
      (unwind-protect
          (let (req)
            (with-current-buffer other (insert "OTHER CONTENT\n"))
            (with-current-buffer origin
              (insert "keep\nTARGET\nkeep\n")
              (let ((start (progn (goto-char (point-min))
                                  (forward-line 1) (point)))
                    (end (line-end-position))
                    (neder-visual-indent nil))
                (setq req (neder-visual start end "replace"))))
            ;; Jump away while the request is in flight.
            (set-buffer other)
            (goto-char (point-max))
            (should (eq 'success (neder-test--wait req)))
            ;; The answer landed in origin...
            (with-current-buffer origin
              (should (string-match-p "LANDED" (buffer-string)))
              (should-not (string-match-p "TARGET" (buffer-string))))
            ;; ...and nothing was written into the buffer we moved to.
            (with-current-buffer other
              (should (equal "OTHER CONTENT\n" (buffer-string)))))
        (kill-buffer origin)
        (kill-buffer other)))))

(ert-deftest neder-test-killed-origin-buffer-fails-cleanly ()
  "Killing the origin buffer mid-request must not error into another buffer."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "sleep 0.4; printf 'IGNORED'"))
    (let ((origin (generate-new-buffer "neder-origin-2"))
          (other (generate-new-buffer "neder-other-2"))
          req)
      (unwind-protect
          (progn
            (with-current-buffer other (insert "SAFE\n"))
            (with-current-buffer origin
              (insert "keep\nTARGET\nkeep\n")
              (let ((start (progn (goto-char (point-min))
                                  (forward-line 1) (point)))
                    (end (line-end-position)))
                (setq req (neder-visual start end "replace"))))
            (kill-buffer origin)
            (set-buffer other)
            (should (eq 'failed (neder-test--wait req)))
            (should (string-match-p "destroyed" (neder-request-error req)))
            (with-current-buffer other
              (should (equal "SAFE\n" (buffer-string)))))
        (when (buffer-live-p origin) (kill-buffer origin))
        (kill-buffer other)))))

(ert-deftest neder-test-destroyed-selection-fails-cleanly ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "sleep 0.4; printf 'IGNORED'"))
    (with-temp-buffer
      (insert "keep\nDELETE ME\nkeep\n")
      (let* ((start (progn (goto-char (point-min))
                           (forward-line 1) (point)))
             (end (line-end-position))
             (req (neder-visual start end "replace")))
        ;; Destroy the buffer the markers point into.
        (erase-buffer)
        (should (eq 'failed (neder-test--wait req)))
        (should (string-match-p "destroyed" (neder-request-error req)))
        (should (equal "" (buffer-string)))))))

(ert-deftest neder-test-empty-response-is-a-failure ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "printf ''"))
    (with-temp-buffer
      (insert "untouched\n")
      (let* ((start (point-min))
             (end (1- (point-max)))
             (req (neder-visual start end "do nothing")))
        (should (eq 'failed (neder-test--wait req)))
        (should (equal "unable to retrieve response" (neder-request-error req)))
        (should (equal "untouched\n" (buffer-string)))))))

(ert-deftest neder-test-nonzero-exit-is-a-failure ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "echo 'boom' >&2; exit 3"))
    (with-temp-buffer
      (insert "untouched\n")
      (let ((req (neder-visual (point-min) (1- (point-max)) "fail")))
        (should (eq 'failed (neder-test--wait req)))
        (should (string-match-p "exit code: 3" (neder-request-error req)))
        (should (string-match-p "boom" (neder-request-error req)))
        (should (equal "untouched\n" (buffer-string)))))))

(ert-deftest neder-test-cancellation ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "sleep 30; printf 'NEVER'"))
    (with-temp-buffer
      (insert "untouched\n")
      (let ((req (neder-visual (point-min) (1- (point-max)) "slow")))
        (should (eq 'requesting (neder-request-state req)))
        (should (= 1 (neder-stop-all-requests)))
        (should (eq 'cancelled (neder-request-state req)))
        ;; Idempotent, like upstream's Prompt:cancel.
        (should (= 0 (neder-stop-all-requests)))
        (neder-test--wait req 5)
        (should-not (neder--active-requests))
        (should (equal "untouched\n" (buffer-string)))))))

(ert-deftest neder-test-sigkill-escalation ()
  "A CLI that ignores SIGTERM must still die.
Upstream sends only SIGTERM and would leave this process running."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "trap '' TERM; sleep 30"))
    (with-temp-buffer
      (insert "x\n")
      (let* ((neder-kill-grace-seconds 0.5)
             (req (neder-visual (point-min) (1- (point-max)) "stubborn"))
             (proc (neder-request-process req)))
        (neder-stop-all-requests)
        (let ((deadline (+ (float-time) 8)))
          (while (and (process-live-p proc) (< (float-time) deadline))
            (accept-process-output nil 0.1)))
        (should-not (process-live-p proc))))))

(ert-deftest neder-test-concurrency-cap ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "sleep 30"))
    (with-temp-buffer
      (insert "a\nb\nc\nd\ne\n")
      ;; Disjoint regions, so the overlap guard does not fire first.
      (let ((neder-max-concurrent-requests 2)
            (one (point-min))
            (two (+ (point-min) 2))
            (three (+ (point-min) 4)))
        (neder-visual one (1+ one) "one")
        (neder-visual two (1+ two) "two")
        (should-error (neder-visual three (1+ three) "three")
                      :type 'user-error)))))

(ert-deftest neder-test-search-renders-grep-buffer ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c"
              "printf '/tmp/a.el:12:3,2,first note, with comma\\n/tmp/b.el:40:1,1,second\\n'"))
    (let ((req (neder-search "where is it")))
      (should (eq 'success (neder-test--wait req)))
      (with-current-buffer "*neder-search*"
        (should (derived-mode-p 'grep-mode))
        ;; Reshaped to PATH:LNUM:COL:TEXT so grep's regexps match.
        (should (string-match-p "^/tmp/a\\.el:12:3:\\[\\+2\\] first note, with comma$"
                                (buffer-string)))
        (should (string-match-p "2 results" (buffer-string))))
      (kill-buffer "*neder-search*"))))

(ert-deftest neder-test-search-with-no-hits-pops-no-buffer ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "printf 'I could not find anything relevant.\\n'"))
    (ignore-errors (kill-buffer "*neder-search*"))
    (let ((req (neder-search "nothing")))
      (should (eq 'success (neder-test--wait req)))
      (should-not (get-buffer "*neder-search*")))))

(ert-deftest neder-test-visual-requires-region ()
  (with-temp-buffer
    (insert "text\n")
    (deactivate-mark)
    (should-error (call-interactively #'neder-visual) :type 'user-error)))


;;;; Structured response parsing

(ert-deftest neder-test-claude-parses-json-result ()
  (let ((parsed (neder--claude-parse-response
                 "{\"result\":\"THE ANSWER\",\"total_cost_usd\":0.0123,\
\"session_id\":\"abc\",\"num_turns\":2,\"is_error\":false}")))
    (should (equal (plist-get parsed :text) "THE ANSWER"))
    (should (equal (plist-get parsed :cost) 0.0123))
    (should (equal (plist-get parsed :session-id) "abc"))
    (should (equal (plist-get parsed :turns) 2))
    (should-not (plist-get parsed :error))))

(ert-deftest neder-test-claude-parse-falls-back-to-raw ()
  "A non-JSON payload must degrade to the raw text, not be lost."
  (should (equal (plist-get (neder--claude-parse-response "just prose") :text)
                 "just prose")))

(ert-deftest neder-test-claude-parse-reports-is-error ()
  (let ((parsed (neder--claude-parse-response
                 "{\"result\":\"rate limited\",\"is_error\":true}")))
    (should (equal (plist-get parsed :error) "rate limited"))))

(ert-deftest neder-test-json-envelope-strips-preamble ()
  "The whole point of the envelope: prose around the answer cannot land
in the buffer, because the answer is a field rather than the output."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c"
              "printf 'I will now help you with that!\\n{\"result\":\"CLEAN\"}'"))
    ;; Give the stub provider the claude parser.
    (setf (neder-provider-parse-response (neder--provider))
          #'neder--claude-parse-response)
    (with-temp-buffer
      (insert "old\n")
      (let* ((neder-visual-indent nil)
             (req (neder-visual (point-min) (1- (point-max)) "replace")))
        (should (eq 'success (neder-test--wait req)))
        ;; The parser sees a payload with a preamble, fails to parse it,
        ;; and returns it verbatim rather than inventing an answer.
        (should (string-match-p "CLEAN" (buffer-string)))))))

(ert-deftest neder-test-metadata-recorded-on-request ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c"
              "printf '{\"result\":\"X\",\"total_cost_usd\":0.5,\"num_turns\":3}'"))
    (setf (neder-provider-parse-response (neder--provider))
          #'neder--claude-parse-response)
    (with-temp-buffer
      (insert "old\n")
      (let* ((neder-visual-indent nil)
             (req (neder-visual (point-min) (1- (point-max)) "replace")))
        (should (eq 'success (neder-test--wait req)))
        (should (equal 0.5 (neder-request-cost req)))
        (should (equal 3 (neder-request-turns req)))))))

(ert-deftest neder-test-provider-error-with-zero-exit-is-a-failure ()
  "The CLI can exit 0 and still report failure inside the envelope."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "printf '{\"result\":\"boom\",\"is_error\":true}'"))
    (setf (neder-provider-parse-response (neder--provider))
          #'neder--claude-parse-response)
    (with-temp-buffer
      (insert "untouched\n")
      (let ((req (neder-visual (point-min) (1- (point-max)) "x")))
        (should (eq 'failed (neder-test--wait req)))
        (should (string-match-p "boom" (neder-request-error req)))
        (should (equal "untouched\n" (buffer-string)))))))


;;;; Mid-request edit detection

(ert-deftest neder-test-changed-selection-aborts-when-declined ()
  "Editing inside the region mid-request must not be silently discarded.
Markers cannot catch this: they track the edit and stay valid, so the
text itself is the only signal."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "sleep 0.4; printf 'FROM MODEL'"))
    (with-temp-buffer
      (insert "keep\nORIGINAL\nkeep\n")
      (let* ((start (progn (goto-char (point-min)) (forward-line 1) (point)))
             (end (line-end-position))
             (neder-confirm-on-changed-selection t)
             (req (neder-visual start end "rewrite")))
        ;; Hand-edit inside the tracked region while the request runs.
        (goto-char end)
        (insert " EDITED BY HAND")
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (should (eq 'failed (neder-test--wait req))))
        (should (string-match-p "selection changed" (neder-request-error req)))
        ;; The hand edit survived and the model's answer was dropped.
        (should (string-match-p "EDITED BY HAND" (buffer-string)))
        (should-not (string-match-p "FROM MODEL" (buffer-string)))))))

(ert-deftest neder-test-changed-selection-applies-when-confirmed ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "sleep 0.4; printf 'FROM MODEL'"))
    (with-temp-buffer
      (insert "keep\nORIGINAL\nkeep\n")
      (let* ((start (progn (goto-char (point-min)) (forward-line 1) (point)))
             (end (line-end-position))
             (neder-visual-indent nil)
             (neder-confirm-on-changed-selection t)
             (req (neder-visual start end "rewrite")))
        (goto-char end)
        (insert " EDITED")
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (should (eq 'success (neder-test--wait req))))
        (should (string-match-p "FROM MODEL" (buffer-string)))))))

(ert-deftest neder-test-unchanged-selection-asks-nothing ()
  "The confirmation must not fire when the region was left alone."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "printf 'NEW'"))
    (with-temp-buffer
      (insert "keep\nORIGINAL\nkeep\n")
      (let* ((start (progn (goto-char (point-min)) (forward-line 1) (point)))
             (end (line-end-position))
             (neder-visual-indent nil)
             (neder-confirm-on-changed-selection t)
             (asked nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (setq asked t) t)))
          (let ((req (neder-visual start end "rewrite")))
            (should (eq 'success (neder-test--wait req)))))
        (should-not asked)
        (should (string-match-p "NEW" (buffer-string)))))))


;;;; Apply styles

(ert-deftest neder-test-confirm-style-applies-on-yes ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe) (list "sh" "-c" "printf 'ACCEPTED'"))
    (with-temp-buffer
      (insert "OLD\n")
      (let ((neder-visual-apply-style 'confirm)
            (neder-visual-indent nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (let ((req (neder-visual (point-min) (1- (point-max)) "x")))
            (should (eq 'success (neder-test--wait req)))))
        (should (string-match-p "ACCEPTED" (buffer-string)))))))

(ert-deftest neder-test-confirm-style-leaves-buffer-alone-on-no ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe) (list "sh" "-c" "printf 'REJECTED'"))
    (with-temp-buffer
      (insert "OLD\n")
      (let ((neder-visual-apply-style 'confirm))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (let ((req (neder-visual (point-min) (1- (point-max)) "x")))
            ;; Declining is a normal outcome, not a failure.
            (should (eq 'success (neder-test--wait req)))))
        (should (equal "OLD\n" (buffer-string)))))))

(ert-deftest neder-test-diff-string-shows-both-sides ()
  (let ((diff (neder--diff-string "alpha\n" "beta\n")))
    (should (string-match-p "^-alpha$" diff))
    (should (string-match-p "^\\+beta$" diff))))


;;;; Context scoping

(ert-deftest neder-test-defun-strategy-takes-the-enclosing-function ()
  "The point of `defun' scoping: whole forms, not a window that cuts one
in half."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun before () 1)\n\n"
            "(defun target ()\n  (message \"hi\")\n  42)\n\n"
            "(defun after () 2)\n")
    (goto-char (point-min))
    (search-forward "message")
    (let* ((neder-context-strategy 'defun)
           (start (match-beginning 0))
           (end (match-end 0))
           (ctx (car (neder--surrounding-context start end))))
      (should (string-match-p "defun target" ctx))
      (should-not (string-match-p "defun before" ctx))
      (should-not (string-match-p "defun after" ctx)))))

(ert-deftest neder-test-defun-strategy-falls-back-to-lines ()
  "Outside any defun the line window must still be produced."
  (with-temp-buffer
    (fundamental-mode)
    (insert "alpha\nbeta\ngamma\n")
    (let* ((neder-context-strategy 'defun)
           (neder-surrounding-context-lines 1)
           (start (progn (goto-char (point-min)) (forward-line 1) (point)))
           (end (line-end-position))
           (result (neder--surrounding-context start end)))
      (should (string-match-p "beta" (car result)))
      (should (equal "surrounding-lines" (cdr result))))))

(ert-deftest neder-test-lines-strategy-matches-upstream-window ()
  (with-temp-buffer
    (insert (mapconcat #'number-to-string (number-sequence 1 50) "\n"))
    (let* ((neder-context-strategy 'lines)
           (neder-surrounding-context-lines 2)
           (start (progn (goto-char (point-min)) (forward-line 24) (point)))
           (end (line-end-position))
           (result (neder--surrounding-context start end)))
      (should (equal "surrounding-lines" (cdr result)))
      (should (string-match-p "23\n24\n25\n26\n27" (car result))))))

(ert-deftest neder-test-visual-prompt-carries-the-language ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(+ 1 2)\n")
    (let* ((neder-use-prompt-buffer nil)
           (neder-context-strategy 'lines)
           (req (neder--request-create
                 :id 0 :op 'visual :directory default-directory
                 :user-prompt "rewrite"
                 :selection "(+ 1 2)" :location "f.el:1:1-1:8"
                 :surrounding "(+ 1 2)"
                 :major-mode-name 'emacs-lisp-mode))
           (text (funcall (neder-op-op-prompt (neder--op 'visual)) req)))
      (should (string-match-p "<LANGUAGE>emacs-lisp</LANGUAGE>" text)))))


;;;; Skills and file references

(defmacro neder-test--with-skill-dir (spec &rest body)
  "Create a temp project with skills from SPEC and run BODY in it.
SPEC is a list of (NAME . CONTENT)."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory (make-temp-file "neder-skills" t)))
          (skills (expand-file-name ".agents/skills/" root)))
     (unwind-protect
         (let ((default-directory root)
               (neder-skill-directories '(".agents/skills/")))
           (dolist (entry ,spec)
             (let ((dir (expand-file-name (car entry) skills)))
               (make-directory dir t)
               (write-region (cdr entry) nil
                             (expand-file-name "SKILL.md" dir) nil 'silent)))
           ,@body)
       (delete-directory root t))))

(ert-deftest neder-test-skills-are-discovered ()
  (neder-test--with-skill-dir '(("tdd" . "red green refactor")
                               ("pr" . "pull request shape"))
    (let ((names (mapcar #'car (neder--skills))))
      (should (member "tdd" names))
      (should (member "pr" names)))))

(ert-deftest neder-test-skill-reference-is-injected ()
  "A #name in the prompt must arrive as a tagged block, as upstream does."
  (neder-test--with-skill-dir '(("tdd" . "RED GREEN REFACTOR"))
    (let* ((neder-context-file-names nil)
           (neder-unsafe-skip-permissions nil)
           (req (neder--request-create :id 0 :op 'search
                                      :directory default-directory
                                      :user-prompt "fix the bug using #tdd")))
      (neder--push-context req (neder--op-prompt "OP" "fix the bug using #tdd"))
      (let ((prompt (neder--finalize-prompt req)))
        (should (string-match-p "<tdd>" prompt))
        (should (string-match-p "RED GREEN REFACTOR" prompt))
        (should (string-match-p "</tdd>" prompt))))))

(ert-deftest neder-test-unknown-skill-reference-is-ignored ()
  (neder-test--with-skill-dir '(("tdd" . "content"))
    (let* ((neder-context-file-names nil)
           (req (neder--request-create :id 0 :op 'search
                                      :directory default-directory
                                      :user-prompt "use #nosuchskill please")))
      (neder--push-context req (neder--op-prompt "OP" "use #nosuchskill please"))
      ;; No block, and crucially no error: an unresolved reference is
      ;; just a word in the prompt.
      (should-not (string-match-p "<nosuchskill>"
                                  (neder--finalize-prompt req))))))

(ert-deftest neder-test-project-local-skill-shadows-global ()
  (neder-test--with-skill-dir '(("tdd" . "LOCAL VERSION"))
    (let* ((global (file-name-as-directory (make-temp-file "neder-global" t)))
           (dir (expand-file-name "tdd" global)))
      (unwind-protect
          (progn
            (make-directory dir t)
            (write-region "GLOBAL VERSION" nil
                          (expand-file-name "SKILL.md" dir) nil 'silent)
            (let ((neder-skill-directories (list ".agents/skills/" global)))
              (should (string-match-p
                       "LOCAL VERSION"
                       (car (neder--resolve-skills "use #tdd"))))))
        (delete-directory global t)))))

(ert-deftest neder-test-file-reference-is-injected ()
  (let* ((root (file-name-as-directory (make-temp-file "neder-files" t))))
    (unwind-protect
        (let ((default-directory root)
              (neder-reference-files t))
          (write-region "FILE BODY" nil (expand-file-name "notes.md" root)
                        nil 'silent)
          (let ((blocks (neder--resolve-files "look at @notes.md")))
            (should (= 1 (length blocks)))
            (should (string-match-p "<File path=\"notes.md\">" (car blocks)))
            (should (string-match-p "FILE BODY" (car blocks)))))
      (delete-directory root t))))

(ert-deftest neder-test-oversized-file-reference-is-skipped ()
  (let* ((root (file-name-as-directory (make-temp-file "neder-big" t))))
    (unwind-protect
        (let ((default-directory root)
              (neder-max-reference-file-bytes 16))
          (write-region (make-string 1000 ?x) nil
                        (expand-file-name "big.txt" root) nil 'silent)
          (should-not (neder--resolve-files "read @big.txt")))
      (delete-directory root t))))

(ert-deftest neder-test-prompt-tokens-are-distinct ()
  (should (equal (neder--prompt-tokens "use #a and #b and #a again" "#")
                 '("a" "b")))
  (should (equal (neder--prompt-tokens "no references here" "#") nil)))


;;;; Prompt buffer and completion

(ert-deftest neder-test-prompt-buffer-returns-its-contents ()
  (let ((neder-use-prompt-buffer t))
    ;; `recursive-edit' cannot run in batch, so drive the mode directly:
    ;; what matters is that send captures the buffer text verbatim.
    (with-temp-buffer
      (neder-prompt-mode)
      (insert "rewrite this using #tdd")
      (cl-letf (((symbol-function 'exit-recursive-edit) #'ignore))
        (neder-prompt-send))
      (should (equal neder--prompt-result "rewrite this using #tdd")))))

(ert-deftest neder-test-prompt-buffer-abort-clears-result ()
  (with-temp-buffer
    (neder-prompt-mode)
    (insert "discard me")
    (cl-letf (((symbol-function 'abort-recursive-edit) #'ignore))
      (neder-prompt-abort))
    (should-not neder--prompt-result)))

(ert-deftest neder-test-skill-capf-completes-after-hash ()
  (neder-test--with-skill-dir '(("tdd" . "x") ("triage" . "y"))
    (with-temp-buffer
      (neder-prompt-mode)
      (insert "use #t")
      (let ((capf (neder-capf-skill)))
        (should capf)
        ;; The completion region starts after the # so the marker is kept.
        (should (equal (buffer-substring (nth 0 capf) (nth 1 capf)) "t"))
        (should (member "tdd" (nth 2 capf)))))))

(ert-deftest neder-test-file-capf-is-nil-without-at-sign ()
  (with-temp-buffer
    (neder-prompt-mode)
    (insert "no reference here")
    (should-not (neder-capf-file))))


;;;; Op registry

(ert-deftest neder-test-ops-are-registered ()
  (should (neder--op 'visual))
  (should (neder--op 'ask))
  (should (neder--op 'search))
  (should (neder-op-track-p (neder--op 'visual)))
  ;; `ask' reads a region but applies nothing, so it must not take markers.
  (should (neder-op-region-p (neder--op 'ask)))
  (should-not (neder-op-track-p (neder--op 'ask)))
  (should-not (neder-op-region-p (neder--op 'search)))
  (should-error (neder--op 'nosuchop)))

(ert-deftest neder-test-neder-run-drives-a-registered-op ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "printf '/tmp/x.el:1:1,1,note\\n'"))
    (ignore-errors (kill-buffer "*neder-search*"))
    (let ((req (neder-run :op 'search :prompt "find it")))
      (should (eq 'success (neder-test--wait req)))
      (should (get-buffer "*neder-search*"))
      (kill-buffer "*neder-search*"))))

(ert-deftest neder-test-neder-run-rejects-region-op-without-region ()
  (should-error (neder-run :op 'visual :prompt "x") :type 'user-error))

(ert-deftest neder-test-custom-op-round-trips ()
  "A user-defined op must need no changes to neder.el itself."
  (let ((neder--ops (copy-alist neder--ops))
        (captured nil))
    (neder-register-op
     (neder-op-create
      :name 'shout
      :region-p nil
      :track-p nil
      :op-prompt (lambda (_req) "<Task>SHOUT</Task>")
      :complete (lambda (_req response) (setq captured response))))
    (neder-test--with-stub
        (lambda (_model prompt _unsafe)
          (should (string-match-p "<Task>SHOUT</Task>" prompt))
          (list "sh" "-c" "printf 'HEARD'"))
      (let ((req (neder-run :op 'shout :prompt "say it")))
        (should (eq 'success (neder-test--wait req)))
        (should (equal "HEARD" captured))))))


;;;; Overlap guard, spinner, history

(ert-deftest neder-test-overlapping-region-is-rejected ()
  "Two tracked requests on one region would race to replace it."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe) (list "sh" "-c" "sleep 30"))
    (with-temp-buffer
      (insert "alpha beta gamma\n")
      (let ((start (point-min))
            (end (+ (point-min) 10)))
        (neder-visual start end "one")
        ;; Fully overlapping.
        (should-error (neder-visual start end "two") :type 'user-error)
        ;; Partially overlapping.
        (should-error (neder-visual (+ start 5) (+ end 3) "three")
                      :type 'user-error)
        ;; Disjoint is fine.
        (should (neder-visual (+ start 11) (+ start 15) "four"))))))

(ert-deftest neder-test-ask-does-not-take-the-overlap-guard ()
  "`ask' tracks nothing, so it must never be blocked by a live visual."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe) (list "sh" "-c" "sleep 30"))
    (with-temp-buffer
      (insert "alpha beta\n")
      (neder-visual (point-min) (+ (point-min) 5) "one")
      (should (neder-ask (point-min) (+ (point-min) 5) "what is this?")))))

(ert-deftest neder-test-spinner-is-removed-when-the-request-settles ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe) (list "sh" "-c" "printf 'DONE'"))
    (with-temp-buffer
      (insert "old\n")
      (let* ((neder-spinner t)
             (neder-visual-indent nil)
             (req (neder-visual (point-min) (1- (point-max)) "x")))
        (should (neder-request-overlay req))
        (should (eq 'success (neder-test--wait req)))
        (should-not (neder-request-overlay req))
        (should-not (neder-request-spinner-timer req))
        ;; No stray overlays left behind in the buffer.
        (should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest neder-test-spinner-is-removed-on-cancel ()
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe) (list "sh" "-c" "sleep 30"))
    (with-temp-buffer
      (insert "old\n")
      (let* ((neder-spinner t)
             (req (neder-visual (point-min) (1- (point-max)) "x")))
        (should (neder-request-overlay req))
        (neder-stop-all-requests)
        (should-not (neder-request-overlay req))))))

(ert-deftest neder-test-previous-search-buffer-is-kept ()
  "Each search used to destroy the last one's results."
  (neder-test--with-stub
      (lambda (_model _prompt _unsafe)
        (list "sh" "-c" "printf '/tmp/a.el:1:1,1,note\\n'"))
    (dolist (buf (buffer-list))
      (when (string-prefix-p "*neder-search" (buffer-name buf))
        (kill-buffer buf)))
    (let ((first (neder-search "one")))
      (should (eq 'success (neder-test--wait first)))
      (let ((second (neder-search "two")))
        (should (eq 'success (neder-test--wait second)))
        ;; The newest keeps the plain name; the older one is still around.
        (should (get-buffer "*neder-search*"))
        (should (seq-find (lambda (b)
                            (string-match-p "\\`\\*neder-search<"
                                            (buffer-name b)))
                          (buffer-list)))))
    (dolist (buf (buffer-list))
      (when (string-prefix-p "*neder-search" (buffer-name buf))
        (kill-buffer buf)))))

(ert-deftest neder-test-request-label-is-one-line ()
  (let ((req (neder--request-create :id 7 :op 'visual :state 'success
                                   :user-prompt "line one\nline two")))
    (let ((label (neder--request-label req)))
      (should-not (string-match-p "\n" label))
      (should (string-match-p "visual" label))
      (should (string-match-p "success" label)))))


;;;; Project root

(ert-deftest neder-test-project-root-never-signals ()
  "A buffer visiting a deleted directory must not break root detection."
  (let ((dir (file-name-as-directory (make-temp-file "neder-gone-root" t))))
    (delete-directory dir t)
    (with-temp-buffer
      (setq-local default-directory dir)
      (should (stringp (neder--project-root))))))

(provide 'neder-tests)
;;; neder-tests.el ends here
