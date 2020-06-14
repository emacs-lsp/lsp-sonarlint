;;; lsp-sonarlint.el --- Emacs Sonarlint lsp client              -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Fermin Munoz

;; Author: Fermin MF <fmfs@posteo.net>
;; Created: 13 Jun 2020
;; Version: 0.0.1
;; Keywords: languages
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

;; This is NOT and official Sonarlint extension.


;;; Code:

(require 'lsp-mode)
(require 'dash)
(require 'cus-edit)
(require 'ht)

(defgroup lsp-sonarlint nil
  "Sonarlint lsp server group"
  :group 'lsp-mode
  :link '(url-link "https://gitlab.com/sasanidas/lsp-sonarlint")
  :package-version '(lsp-mode . "6.4"))

(defcustom lsp-sonarlint-server-path
  (concat
   (file-name-directory load-file-name)
   "server/sonarlint-language-server.jar")
  "Lsp-sonarlint language server location."
  :group 'lsp-sonarlint
  :type 'file)

(defcustom lsp-sonarlint-modes-enabled '(php-mode html-mode web-mode js-mode js2-mode python-mode java-mode css-mode ruby-mode scala-mode xml-mode nxml-mode)
  "Lsp-sonarlint activation modes."
  :group 'lsp-sonarlint
  :type 'file)

(defcustom lsp-sonarlint-disable-telemetry t
  "Lsp-sonarlint telemetry option."
  :group 'lsp-sonarlint
  :type 'boolean)

(defcustom lsp-sonarlint-test-file-pattern "{**/test/**,**/*test*,**/*Test*}"
  "Lsp-sonarlint test language pattern."
  :group 'lsp-sonarlint
  :type 'string)

(defcustom lsp-sonarlint-server-download-url
  "https://search.maven.org/remotecontent?filepath=org/sonarsource/sonarlint/core/sonarlint-language-server/4.6.0.2652/sonarlint-language-server-4.6.0.2652.jar"
  "Lsp-sonarlint jar lsp server URL."
  :group 'lsp-sonarlint
  :type 'string)

(let ((default-directory (concat
			  (file-name-directory load-file-name)
			  "languages")))
  (normal-top-level-add-subdirs-to-load-path))

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
	      (progn
		(when (not (file-exists-p
			    enabled-member--analyzer-path))
		  (when (yes-or-no-p "lsp-sonarlint language plugin not found, do you want to download it? ")
		    (shell-command (concat "curl " enabled-member--download-url " --output " enabled-member--analyzer-path))))
		))
	    (concat "file://"  (eval (intern (concat (format "%s" (car enabled-member) ) "-analyzer-path"))) " "))
	  lsp-sonarlint--enabled-plugins)))

(defun lsp-sonarlint--code-action-open-rule (rule)
  "Create an HTML rendered buffer with the RULE text in it."
  (with-temp-buffer
    (let* ((rule-title (format "%s" (aref (ht-get rule "arguments") 1)))
	   (rule-formated-title (replace-regexp-in-string ">" " "
							  (replace-regexp-in-string "<"  " "  rule-title)))
	   (rule-body (aref  (ht-get rule "arguments" ) 2)))
      (progn
	(insert rule-formated-title)
	(insert "\n")
	(insert rule-body)
	))
    (shr-render-buffer (current-buffer)))
  )


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
   ("sonarlint.testFilePattern" lsp-sonarlint-test-file-pattern)))

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
