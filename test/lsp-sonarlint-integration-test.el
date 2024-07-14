;;; integration.el --- Integration tests for SonarLint LSP client   -*- lexical-binding: t; -*-
;;;
;; Author: Arseniy Zaostrovnykh
;; Created: 10 Jun 2023
;; Version: 0.0.1
;; License: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; Package-Requires: ((emacs "27.2"))

;;; Commentary:
;; Tests for the integration of the LSP mode and SonarLint language server
;; through lsp-sonarlint package.

;;; Code:

(require 'lsp-mode)
(require 'lsp-sonarlint)
(load-file (expand-file-name "lsp-sonarlint-test-utils.el"
                             (file-name-directory (or load-file-name (buffer-file-name)))))

(ert-deftest lsp-sonarlint-plugin-downloaded ()
  "Check whether you have downloaded SonarLint.

This is a prerequisite for all the integration tests. If this
test fails, you need to download the SonarLint plugin using

make download-sonarlint"
  (should (file-exists-p (concat lsp-sonarlint-download-dir "/extension/server/sonarlint-ls.jar"))))

(defun lsp-sonarlint--wait-for (predicate hook timeout)
  "Register PREDICATE to run on HOOK, and wait until it returns t.
If that does not occur before TIMEOUT, throw an error."
  (let ((done nil))
    (cl-flet ((setter (lambda (&rest args) (when (apply predicate args)
                                        (setq done t)))))
      (unwind-protect
          (progn
            (add-hook hook #'setter)
            (with-timeout (timeout (error "Timed out waiting for %s" hook))
              (while (not done) (accept-process-output nil 0.1))))
        (remove-hook hook #'setter)))))

(defun lsp-sonarlint--any-alive-workspaces-p ()
  "Whether there are any LSP workspaces present in the session."
  (< 0 (hash-table-count (lsp-session-folder->servers (lsp-session)))))

(defun lsp-sonarlint--wait-for-workspaces-to-die (timeout)
  "Wait until there are no more active LSP workspaces in the session.
If that does not happen within TIMEOUT, throw an error."
  (with-timeout (timeout (error "Timed out waiting for the workspace to shutdown"))
    (while (lsp-sonarlint--any-alive-workspaces-p)
      (accept-process-output nil 0.1))))

(defun lsp-sonarlint--exec-with-diags (file diag-consumer major-mode)
  "Execute DIAG-CONSUMER in the buffer holding FILE.
DIAG-CONSUMER is executed once LSP has some diagnostics for the file,
in the LSP-enabled buffer.
DIAG-CONSUMER must accept 1 argument - the list of diagnostics.
It can perform further interaction with LSP, e.g., execute code actions.
The MAJOR-MODE triggering the analysis matters, because the cfamily analyzer
only works for specific textDocument/didOpen:languageId."
  ;; It is important to start from a clean slate.
  ;; If lsp-mode runs any servers already, the test might fall into a race condition,
  ;; when a server was requested to stop, but did not quite shut down yet,
  ;; lsp-mode might reopen the connection with the new FILE, thus communicating with the
  ;; end-of-life server. This puts lsp-mode into a buggy state - it is a race condition in lsp-mode
  (should (null (lsp-sonarlint--any-alive-workspaces-p)))
  (let ((lsp-enabled-clients '(sonarlint))
        (lsp-keep-workspace-alive nil)
        (dir (file-name-directory file))
        (lsp-enable-snippet nil)
        received-warnings)
    (let ((buf (find-file-noselect file))
          (diagnostics-updated nil)
          (register-warning (lambda (&rest w) (when (equal (car w) 'lsp-mode)
                                           (push (cadr w) received-warnings)))))
      (unwind-protect
          (progn
            (advice-add 'display-warning :before register-warning)
            (lsp-workspace-folders-add dir)
            (with-current-buffer buf
              (funcall major-mode)
              (lsp)
              (lsp-sonarlint--wait-for
               (lambda ()
                 (when-let ((stats (lsp-diagnostics-stats-for file)))
                   (when (< 0 (seq-reduce '+ stats 0))
                     (setq diagnostics-updated (gethash file (lsp-diagnostics t))))))
               'lsp-diagnostics-updated-hook
               40)
              (should (null received-warnings))
              (funcall diag-consumer diagnostics-updated)))
        (kill-buffer buf)
        (lsp-workspace-folders-remove dir)
        (advice-remove 'display-warning register-warning)
        (lsp-sonarlint--wait-for-workspaces-to-die 10)))))

(defun lsp-sonarlint--get-codes-of-issues (issues)
  "Extract the code of each of ISSUES."
  (sort (mapcar (lambda (issue) (gethash "code" issue)) issues) #'string-lessp))


(defun lsp-sonarlint--get-all-issue-codes (sample-filename &optional major-mode)
  "Get all SonarLint issue-codes for given SAMPLE-FILENAME.
This functions takes some time to wait for the LSP mode to init
and get the issues from the server.
MAJOR-MODE specifies the major mode enabled to trigger the analysis.
Some analyzers like cfamily require specific major-modes.
If nil, use python-mode by default."
  (lsp-sonarlint--exec-with-diags
   (lsp-sonarlint-sample-file sample-filename)
   (lambda (diags)
     (lsp-sonarlint--get-codes-of-issues diags))
   (if major-mode major-mode 'python-mode)))

(ert-deftest lsp-sonarlint-python-reports-issues ()
  "Check that LSP can get Python SonarLint issues for a Python file."
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.py")
                 '("python:S1135" "python:S1716"))))

(ert-deftest lsp-sonarlint-java-reports-issues ()
  "Check that LSP can get Java SonarLint issues for a Java file."
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.java")
                 '("java:S106" "java:S1135" "java:S1220"))))

(ert-deftest lsp-sonarlint-html-reports-issues ()
  "Check that LSP can get HTML SonarLint issues for an HTML file."
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.html")
                 '("Web:S1135"))))

;; javascript-sample.js must have a distinct name from sample.ts,
;; otherwise the javascript/typescript plugin gets confused.
(ert-deftest lsp-sonarlint-js-reports-issues ()
  "Check that LSP can get JavaScript SonarLint issues for a JavaScript file."
  (should (equal (lsp-sonarlint--get-all-issue-codes "javascript-sample.js")
                 '("javascript:S1134" "javascript:S1135"))))

(ert-deftest lsp-sonarlint-ts-reports-issues ()
  "Check that LSP can get TypeScript SonarLint issues for a TypeScript file."
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.ts")
                 '("typescript:S1134" "typescript:S1135"))))

(ert-deftest lsp-sonarlint-php-reports-issues ()
  "Check that LSP can get PHP SonarLint issues for a PHP file."
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.php")
                 '("php:S1135" "php:S1780"))))

(ert-deftest lsp-sonarlint-xml-reports-issues ()
  "Check that LSP can get XML SonarLint issues for a XML file."
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.xml")
                 '("xml:S1135"))))

;; "text" plugin detects secrets and bidirectional unicode characters
(ert-deftest lsp-sonarlint-text-reports-issues ()
  "Check that LSP can detect Secrets with SonarLint."
  (let ((lsp-sonarlint-enabled-analyzers '("text")))
    (should (equal (lsp-sonarlint--get-all-issue-codes "secrets.java")
                   '("secrets:S6290" "secrets:S6290" "secrets:S6290")))))

(ert-deftest lsp-sonarlint-go-reports-issues ()
  "Check that LSP can get go SonarLint issues for a go file."
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.go")
                 '("go:S1135"))))

(ert-deftest lsp-sonarlint-c++-reports-issues ()
  "Check that LSP can get go SonarLint issues for a C++ file."
  (should (equal (lsp-sonarlint--get-all-issue-codes "cpp/sample.cpp" 'c++-mode)
                 '("cpp:S995"))))

(defun lsp-sonarlint--find-descr-action-at-point ()
  "Find the `get rule description' code action for the issue at point."
  (seq-find (lambda (action) (string-match-p "description" (gethash "title" action)))
            (lsp-code-actions-at-point)))

(defun lsp-sonarlint--buf-has-rule-descr-p (buf)
  "Check whether the given buffer contains a rule description."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (message "buffer %s : %s" (current-buffer)
               (search-forward "Noncompliant" nil t)))
    (save-excursion
      (goto-char (point-min))
      (search-forward "Noncompliant" nil t))))

(defun lsp-sonarlint--go-to-first-diag (diags)
  "Move point to the location within the first diagnostic of DIAGS."
  (let* ((first-diagnostic (car diags))
         (diag-range (gethash "start" (gethash "range" first-diagnostic)))
         (diag-line (gethash "line" diag-range))
         (diag-col (gethash "character" diag-range)))
    (goto-char (point-min))
    (forward-line diag-line)
    (forward-char diag-col)))

(ert-deftest lsp-sonarlint-display-rule-descr-test ()
  "Check whether you can display rule description for a SonarLint issue."
  (lsp-sonarlint--exec-with-diags
   (lsp-sonarlint-sample-file "sample.py")
   (lambda (diags)
     (lsp-sonarlint--go-to-first-diag diags)
     (let ((descr-action (lsp-sonarlint--find-descr-action-at-point)))
       (let ((description-opened nil))
         (cl-flet ((check-opened-buffer (buf)
                     (when (lsp-sonarlint--buf-has-rule-descr-p buf)
                       (setq description-opened t))))
           (unwind-protect
               (progn
                 (advice-add 'shr-render-buffer :before #'check-opened-buffer)
                 (with-timeout (8 (error "Timeout waiting for rule description"))
                   (while (not description-opened)
                     ;; Repeat the request multiple times because SonarLint
                     ;; might get distracted with other requests and "forget" to
                     ;; respond
                     (lsp-execute-code-action descr-action)
                     (message "still waiting")
                     (sit-for 0.3)))
                 (should description-opened))
             (advice-remove 'shr-render-buffer #'check-opened-buffer))))))
   'python-mode))

;;; integration.el ends here
