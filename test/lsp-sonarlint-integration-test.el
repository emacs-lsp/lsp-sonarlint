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

;;; Commentary:
;; Tests for the integration of the LSP mode and SonarLint language server
;; through lsp-sonarlint package.

;;; Code:

(require 'lsp-mode)
(require 'lsp-sonarlint)

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

(defun lsp-sonarlint--exec-with-diags (file knob-symbol diag-consumer)
  "Execute DIAG-CONSUMER in the buffer holding FILE with KNOB-SYMBOL enabled.
DIAG-CONSUMER is executed once LSP has some diagnostics for the file,
in the LSP-enabled buffer.
DIAG-CONSUMER must accept 1 argument - the list of diagnostics.
It can perform further interaction with LSP, e.g., execute code actions."
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
        ;; Disable all plugins to focus only on the issues from the knob-symbol
        (lsp-sonarlint-go-enabled nil)
        (lsp-sonarlint-html-enabled nil)
        (lsp-sonarlint-java-enabled nil)
        (lsp-sonarlint-javascript-enabled nil)
        (lsp-sonarlint-typescript-enabled nil)
        (lsp-sonarlint-php-enabled nil)
        (lsp-sonarlint-text-enabled nil)
        (lsp-sonarlint-typescript-enabled nil)
        (lsp-sonarlint-xml-enabled nil)
        received-warnings)
    (let ((buf (find-file-noselect file))
          (lsp-sonarlint-plugin-autodownload t)
          (register-warning (lambda (&rest w) (when (equal (car w) 'lsp-mode)
                                           (push (cadr w) received-warnings)))))
      (unwind-protect
          (progn
            (advice-add 'display-warning :before register-warning)
            (lsp-workspace-folders-add dir)
            (with-current-buffer buf
              (cl-letf (((symbol-value knob-symbol) t))
                (python-mode) ;; Any prog mode that triggers lsp-sonarlint triggers all its analyzers
                (lsp))
              (lsp-sonarlint--wait-for
               (lambda ()
                 (when-let ((stats (lsp-diagnostics-stats-for file)))
                   (when (< 0 (seq-reduce '+ stats 0))
                     (setq diagnostics-updated t))))
               'lsp-diagnostics-updated-hook
               40)
              (should (null received-warnings))
              (funcall diag-consumer (gethash file (lsp-diagnostics t)))))
        (kill-buffer buf)
        (lsp-workspace-folders-remove dir)
        (advice-remove 'display-warning register-warning)
        (lsp-sonarlint--wait-for-workspaces-to-die 10)))))

(defun lsp-sonarlint--get-codes-of-issues (issues)
  "Extract the code of each of ISSUES."
  (sort (mapcar (lambda (issue) (gethash "code" issue)) issues) #'string-lessp))


(defun lsp-sonarlint--fixtures-dir ()
  "Directory of the test fixtures for these tests."
  (concat
   (file-name-directory
    (directory-file-name (file-name-directory (symbol-file #'lsp-sonarlint--fixtures-dir))))
   "fixtures/"))

(defun lsp-sonarlint--sample-file (fname)
  "Get the full path of the sample file FNAME."
  (concat (lsp-sonarlint--fixtures-dir) fname))

(defun lsp-sonarlint--get-all-issue-codes (sample-filename knob-symbol)
  "Get all SonarLint issue-codes for given SAMPLE-FILENAME with KNOB-SYMBOL on.
This functions takes some time to wait for the LSP mode to init
and get the issues from the server."
  (lsp-sonarlint--exec-with-diags
   (lsp-sonarlint--sample-file sample-filename) knob-symbol
   (lambda (diags)
     (lsp-sonarlint--get-codes-of-issues diags))))

(ert-deftest lsp-sonarlint-python-reports-issues ()
  "Check that LSP can get Python SonarLint issues for a Python file."
  (require 'lsp-sonarlint-python)
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.py" 'lsp-sonarlint-python-enabled)
                 '("python:S1135" "python:S1716"))))

(ert-deftest lsp-sonarlint-java-reports-issues ()
  "Check that LSP can get Java SonarLint issues for a Java file."
  (require 'lsp-sonarlint-java)
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.java" 'lsp-sonarlint-java-enabled)
                 '("java:S106" "java:S1135" "java:S1220"))))

(ert-deftest lsp-sonarlint-html-reports-issues ()
  "Check that LSP can get HTML SonarLint issues for an HTML file."
  (require 'lsp-sonarlint-html)
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.html" 'lsp-sonarlint-html-enabled)
                 '("Web:S1135"))))

;; javascript-sample.js must have a distinct name from sample.ts,
;; otherwise the javascript/typescript plugin gets confused.
(ert-deftest lsp-sonarlint-js-reports-issues ()
  "Check that LSP can get JavaScript SonarLint issues for a JavaScript file."
  (require 'lsp-sonarlint-javascript)
  (should (equal (lsp-sonarlint--get-all-issue-codes "javascript-sample.js" 'lsp-sonarlint-javascript-enabled)
                 '("javascript:S1134" "javascript:S1135"))))

(ert-deftest lsp-sonarlint-ts-reports-issues ()
  "Check that LSP can get TypeScript SonarLint issues for a TypeScript file."
  (require 'lsp-sonarlint-typescript)
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.ts" 'lsp-sonarlint-typescript-enabled)
                 '("typescript:S1134" "typescript:S1135"))))

(ert-deftest lsp-sonarlint-php-reports-issues ()
  "Check that LSP can get PHP SonarLint issues for a PHP file."
  (require 'lsp-sonarlint-php)
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.php" 'lsp-sonarlint-php-enabled)
                 '("php:S1135" "php:S1780"))))

(ert-deftest lsp-sonarlint-xml-reports-issues ()
  "Check that LSP can get XML SonarLint issues for a XML file."
  (require 'lsp-sonarlint-xml)
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.xml" 'lsp-sonarlint-xml-enabled)
                 '("xml:S1135"))))

;; "text" plugin detects secrets and bidirectional unicode characters
(ert-deftest lsp-sonarlint-text-reports-issues ()
  "Check that LSP can detect Secrets with SonarLint."
  (require 'lsp-sonarlint-text)
  (should (equal (lsp-sonarlint--get-all-issue-codes "secrets.java" 'lsp-sonarlint-text-enabled)
                 '("secrets:S6290" "secrets:S6290" "secrets:S6290"))))

(ert-deftest lsp-sonarlint-go-reports-issues ()
  "Check that LSP can get go SonarLint issues for a go file."
  (require 'lsp-sonarlint-go)
  (should (equal (lsp-sonarlint--get-all-issue-codes "sample.go" 'lsp-sonarlint-go-enabled)
                 '("go:S1135"))))

(defun lsp-sonarlint--find-descr-action-at-point ()
  "Find the 'get rule description' code action for the issue at point."
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
  (require 'lsp-sonarlint-python)
  (lsp-sonarlint--exec-with-diags
   (lsp-sonarlint--sample-file "sample.py")
   'lsp-sonarlint-python-enabled
   (lambda (diags)
     (lsp-sonarlint--go-to-first-diag diags)
     (let ((descr-action (lsp-sonarlint--find-descr-action-at-point)))
       (let ((description-opened nil))
         (cl-flet ((check-opened-buffer
                    (buf)
                    (when (lsp-sonarlint--buf-has-rule-descr-p buf)
                      (setq description-opened t))))
           (unwind-protect
               (progn
                 (advice-add 'shr-render-buffer :before #'check-opened-buffer)
                 (lsp-execute-code-action descr-action)
                 (with-timeout (8 (error "Timeout waiting for rule description"))
                   (while (not description-opened)
                     (message "still waiting")
                     (sit-for 0.1)))
                 (should description-opened))
             (advice-remove 'shr-render-buffer #'check-opened-buffer))))))))
