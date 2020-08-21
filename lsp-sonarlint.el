;;; lsp-sonarlint.el --- Emacs Sonarlint lsp client              -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Fermin Munoz

;; Author: Fermin MF <fmfs@posteo.net>
;; Created: 13 Jun 2020
;; Version: 0.0.1
;; Keywords: languages, tools, php, javascript, xml, ruby, html, scala, java, python
;; URL: https://gitlab.com/sasanidas/lsp-sonarlint
;; Package-Requires: ((emacs "25") (dash "2.12.0") (lsp-mode "6.3") (ht "2.3"))
;; License: GPL-3.0-or-later

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
;; Sonarlint LSP extensions for GNU Emacs, add support for the majority of sonarlint languages
;; including: php, javascript, typescript, html, python and java.

;; This is NOT an official Sonarlint extension.


;;; Code:

(require 'lsp-mode)
(require 'dash)
(require 'cus-edit)
(require 'ht)
(require 'shr)

(defgroup lsp-sonarlint nil
  "Sonarlint lsp server group"
  :group 'lsp-mode
  :link '(url-link "https://gitlab.com/sasanidas/lsp-sonarlint")
  :package-version '(lsp-mode . "6.4"))

(defcustom lsp-sonarlint-server-path
  (concat
   (file-name-directory load-file-name)
   "server/sonarlint-language-server.jar")
  "Sonarlint Language Server jar file location."
  :group 'lsp-sonarlint
  :type 'file)

(defcustom lsp-sonarlint-modes-enabled '(php-mode html-mode web-mode js-mode js2-mode python-mode java-mode ruby-mode scala-mode xml-mode nxml-mode)
  "List of enabled major modes."
  :group 'lsp-sonarlint
  :type 'file)

(defcustom lsp-sonarlint-disable-telemetry t
  "Disable sending anonymous usage statistics to SonarSource.
To see a sample of the data that are collected
https://github.com/SonarSource/sonarlint-vscode/blob/master/telemetry-sample.md."
  :group 'lsp-sonarlint
  :type 'boolean)

(defcustom lsp-sonarlint-test-file-pattern "{**/test/**,**/*test*,**/*Test*}"
  "Files whose name match java global are considered as test files by analyzers.
Most rules are not evaluated on test files.
Example: `{**/test/**,**/*test*,**/*Test*}`"
  :group 'lsp-sonarlint
  :type 'string)

(defcustom lsp-sonarlint-sonarqube-server-url ""
  "URL of the server.
e.g https://<myServerUrl>"
  :group 'lsp-sonarlint
  :type 'string)

(defcustom lsp-sonarlint-show-analyzer-logs nil
  "Show analyzer's logs in the SonarLint output."
  :group 'lsp-sonarlint
  :type 'boolean)

(defcustom lsp-sonarlint-vmargs ""
  "Extra JVM arguments used to launch the SonarLint LSP.
e.g. `-Xmx1024m`."
  :group 'lsp-sonarlint
  :type 'string)

(defcustom lsp-sonarlint-server-download-url
  "https://binaries.sonarsource.com/Distribution/sonarlint-language-server/sonarlint-language-server-4.6.0.2652.jar"
  "Sonarlint Language Server jar file download URL."
  :group 'lsp-sonarlint
  :type 'string)

(let ((languages-directory-path (concat (file-name-directory load-file-name) "languages")))
  (if (file-directory-p languages-directory-path)
      (add-to-list 'load-path languages-directory-path)
    (error "There were and error with the `load-file-name` function")))

(defun lsp-sonarlint--plugin-list ()
  "Check for the enabled extensions and return a path list.
If the analyzer path is not a file, it ask for download the
analyzer"
  (let* ((lsp-sonarlint--enabled-plugins
	  (-filter (lambda (member)
		     (when (eval
			    (intern (concat (format "%s" (car member) ) "-enabled")))
		       t))
		   (custom-group-members 'lsp-sonarlint t))))

    (-map (lambda (enabled-member)
	    (let* ((enabled-member--download-url
		    (eval (intern (concat (format "%s" (car enabled-member) ) "-download-url"))))
		   (enabled-member--analyzer-path
		    (eval (intern (concat (format "%s" (car enabled-member) ) "-analyzer-path")))))
	      (unless (file-exists-p
		       enabled-member--analyzer-path)
		(when (yes-or-no-p "lsp-sonarlint language plugin not found, do you want to download it? ")
		  (url-copy-file enabled-member--download-url enabled-member--analyzer-path)))
	      (concat "file://"  enabled-member--analyzer-path " ")))
	  lsp-sonarlint--enabled-plugins)))

(defun lsp-sonarlint--code-action-open-rule (rule)
  "Create an HTML rendered buffer with the RULE text in it."
  (with-temp-buffer
    (let* ((rule-title (format "%s" (aref (ht-get rule "arguments") 1)))
	   (rule-formated-title (replace-regexp-in-string ">" " "
							  (replace-regexp-in-string "<"  " "  rule-title)))
	   (rule-body (aref  (ht-get rule "arguments" ) 2)))
      (insert rule-formated-title)
       (insert "\n")
       (insert rule-body))
    (shr-render-buffer (current-buffer))))


(defun lsp-sonarlint-server-start-fun (port)
  "Lsp-sonarlint start function, it need PORT as parameter."
  (-concat
   `("java" "-jar" ,(eval  lsp-sonarlint-server-path )  ,(number-to-string port))
   (lsp-sonarlint--plugin-list)))


(defconst lsp-sonarlint--action-handlers
  '(("SonarLint.OpenRuleDesc" .
     (lambda (rule) (lsp-sonarlint--code-action-open-rule rule)))))


(lsp-register-custom-settings
 '(("sonarlint.disableTelemetry" lsp-sonarlint-disable-telemetry)
   ("sonarlint.testFilePattern" lsp-sonarlint-test-file-pattern)
   ("sonarlint.output.showAnalyzerLogs" lsp-sonarlint-show-analyzer-logs)
   ("sonarlint.ls.vmargs" lsp-sonarlint-vmargs)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-server-command 'lsp-sonarlint-server-start-fun)
  :major-modes lsp-sonarlint-modes-enabled
  :priority -1
  :multi-root t
  :add-on? t
  :server-id 'sonarlint
  :action-handlers (ht<-alist lsp-sonarlint--action-handlers)
  :initialization-options (lambda ()
			    (list
			     :productKey "emacs"
			     :productName "Emacs"))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "sonarlint"))))))

(provide 'lsp-sonarlint)
;;; lsp-sonarlint.el ends here
