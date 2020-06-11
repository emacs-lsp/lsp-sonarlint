;;; lsp-sonarlint.el --- Emacs Sonarlint lsp client              -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Fermin Munoz

;; Author: Fermin MF <fmfs@posteo.net>
;; Created: 13 Jun 2020
;; Version: 0.0.1
;; Keywords: languages
;; URL: https://gitlab.com/sasanidas/lsp-sonarlint
;; Package-Requires: ((emacs "25") (dash "2.12.0") (lsp-mode "6.3"))
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

(defgroup lsp-sonarlint nil
  "Sonarlint lsp server group"
  :group 'lsp-mode
  :link '(url-link "")
  :package-version '(lsp-mode . "6.4"))

(defcustom lsp-sonarlint-server-path
  (concat
   (file-name-directory load-file-name)
   "server/sonarlint-server.jar")
  ;; "/home/fermin/Programming/sonarlint-lsp/sonarlint-server.jar"
 "Sonarlint language server location."
 :group 'lsp-sonarlint
 :type 'file)

(defcustom lsp-sonarlint-telemetry-enabled nil
  "Sonarlint telemetry option."
  :group 'lsp-sonarlint
  :type 'boolean)

(defcustom lsp-sonarlint-test-file-pattern "{**/test/**,**/*test*,**/*Test*}"
  "Sonarlint test language pattern."
  :group 'lsp-sonarlint
  :type 'string)

(defcustom lsp-sonarlint-server-download-url
  "https://search.maven.org/remotecontent?filepath=org/sonarsource/sonarlint/core/sonarlint-language-server/4.6.0.2652/sonarlint-language-server-4.6.0.2652.jar"
  "Sonarlint jar lsp server URL."
  :group 'lsp-sonarlint
  :type 'string)


(defgroup lsp-sonarlint-php nil
  "Sonarlint  analyzer group"
  :group 'lsp-sonarlint)

(defcustom lsp-sonarlint-php-files-extension '("php")
  "Sonarlint php file extensions."
  :group 'lsp-sonarlint-php
  :type 'list)

(defcustom lsp-sonarlint-php-analyzer-path
  "/home/fermin/Programming/sonarlint-lsp/extension/analyzers/sonarphp.jar"
  "Sonarlint php analyzer location."
  :group 'lsp-sonarlint-php
  :type 'file)



(defgroup lsp-sonarlint-html nil
  "Sonarlint html analyzer group"
  :group 'lsp-sonarlint)

(defcustom lsp-sonarlint-html-files-extension '("html")
  "Sonarlint html file extensions."
  :group 'lsp-sonarlint-html
  :type 'list)

(defcustom lsp-sonarlint-html-analyzer-path
  "/home/fermin/Programming/sonarlint-lsp/extension/analyzers/sonarhtml.jar"
  "Sonarlint html analyzer location."
  :group 'lsp-sonarlint-html
  :type 'file)



(defgroup lsp-sonarlint-python nil
  "Sonarlint python analyzer group"
  :group 'lsp-sonarlint)

(defcustom lsp-sonarlint-python-files-extension '("py")
  "Sonarlint python file extensions."
  :group 'lsp-sonarlint-html
  :type 'list)

(defcustom lsp-sonarlint-python-analyzer-path
  "/home/fermin/Programming/sonarlint-lsp/extension/analyzers/sonarpython.jar"
  "Sonarlint python analyzer location."
  :group 'lsp-sonarlint-html
  :type 'file)



(defgroup lsp-sonarlint-javascript nil
  "Sonarlint python analyzer group"
  :group 'lsp-sonarlint)

(defcustom lsp-sonarlint-javascript-files-extension '("js")
  "Sonarlint python file extensions."
  :group 'lsp-sonarlint-javascript
  :type 'list)

(defcustom lsp-sonarlint-javascript-analyzer-path
  "/home/fermin/Programming/sonarlint-lsp/extension/analyzers/sonarjs.jar"
  "Sonarlint python analyzer location."
  :group 'lsp-sonarlint-javascript
  :type 'file)



(defun lsp-sonarlint-server-start-fun (port)
  "Define serenata start function, it requires a PORT."
  `("java" "-jar" ,(eval  lsp-sonarlint-server-path )  ,(number-to-string port)
    ,(concat "file://" lsp-sonarlint-html-analyzer-path " ")
    ,(concat "file://" lsp-sonarlint-php-analyzer-path " ")
    ,(concat "file://" lsp-sonarlint-python-analyzer-path " ")
    ,(concat "file://" lsp-sonarlint-javascript-analyzer-path)))


(lsp-register-custom-settings
 '(("sonarlint.disableTelemetry" t)
   ("sonarlint.testFilePattern" lsp-sonarlint-test-file-pattern)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-server-command 'lsp-sonarlint-server-start-fun)
  :major-modes '(php-mode html-mode web-mode js-mode js2-mode python-mode)
  :priority -1
  :multi-root t
  :add-on? t
  :server-id 'sonarlint
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
