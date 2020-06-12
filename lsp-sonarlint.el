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
  :link '(url-link "https://gitlab.com/sasanidas/lsp-sonarlint")
  :package-version '(lsp-mode . "6.4"))

(defcustom lsp-sonarlint-server-path
  (concat
   (file-name-directory load-file-name)
   "server/sonarlint-server.jar")
 "Lsp-sonarlint language server location."
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


(defgroup lsp-sonarlint-php nil
  "Lsp-sonarlint php analyzer group"
  :group 'lsp-sonarlint)

(defcustom lsp-sonarlint-php-enabled t
  "Lps-sonarlint php analyzer option."
  :group 'lsp-sonarlint-php
  :type 'boolean)

(defcustom lsp-sonarlint-php-analyzer-path
  "/home/fermin/Programming/sonarlint-lsp/extension/analyzers/sonarphp.jar"
  "Lsp-sonarlint php analyzer location."
  :group 'lsp-sonarlint-php
  :type 'file)



(defgroup lsp-sonarlint-html nil
  "Lsp-sonarlint html analyzer group"
  :group 'lsp-sonarlint)

(defcustom lsp-sonarlint-html-enabled t
  "Lps-sonarlint html analyzer option."
  :group 'lsp-sonarlint-html
  :type 'boolean)

(defcustom lsp-sonarlint-html-analyzer-path
  "/home/fermin/Programming/sonarlint-lsp/extension/analyzers/sonarhtml.jar"
  "Lsp-sonarlint html analyzer location."
  :group 'lsp-sonarlint-html
  :type 'file)



(defgroup lsp-sonarlint-python nil
  "lsp-sonarlint python analyzer group"
  :group 'lsp-sonarlint)

(defcustom lsp-sonarlint-python-enabled nil
  "Lps-sonarlint python analyzer option."
  :group 'lsp-sonarlint-python
  :type 'boolean)

(defcustom lsp-sonarlint-python-analyzer-path
  "/home/fermin/Programming/sonarlint-lsp/extension/analyzers/sonarpython.jar"
  "Lsp-sonarlint python analyzer location."
  :group 'lsp-sonarlint-html
  :type 'file)


(defgroup lsp-sonarlint-javascript nil
  "lsp-sonarlint javascript analyzer group"
  :group 'lsp-sonarlint)

(defcustom lsp-sonarlint-javascript-enabled nil
  "Lps-sonarlint javascript analyzer option."
  :group 'lsp-sonarlint-javascript
  :type 'boolean)

(defcustom lsp-sonarlint-javascript-analyzer-path
  "/home/fermin/Programming/sonarlint-lsp/extension/analyzers/sonarjs.jar"
  "Lsp-sonarlint javascript analyzer location."
  :group 'lsp-sonarlint-javascript
  :type 'file)


(defgroup lsp-sonarlint-java nil
  "lsp-sonarlint java analyzer group"
  :group 'lsp-sonarlint)

(defcustom lsp-sonarlint-java-enabled nil
  "Lps-sonarlint java analyzer option."
  :group 'lsp-sonarlint-java
  :type 'boolean)

(defcustom lsp-sonarlint-java-analyzer-path
  "/home/fermin/Programming/sonarlint-lsp/extension/analyzers/sonarjava.jar"
  "Lsp-sonarlint java analyzer location."
  :group 'lsp-sonarlint-java
  :type 'file)



(defun lsp-sonarlint-server-start-fun (port)
  "Lsp-sonarlint start function, it need PORT as parameter."
  (-non-nil
  `("java" "-jar" ,(eval  lsp-sonarlint-server-path )  ,(number-to-string port)
    ,(when lsp-sonarlint-php-enabled
	 (concat "file://" lsp-sonarlint-php-analyzer-path " "))

    ,(when lsp-sonarlint-html-enabled
	 (concat "file://" lsp-sonarlint-html-analyzer-path " "))

    ,(when lsp-sonarlint-python-enabled
	 (concat "file://" lsp-sonarlint-python-analyzer-path " "))

    ,(when lsp-sonarlint-javascript-enabled
	 (concat "file://" lsp-sonarlint-javascript-analyzer-path " "))

    ,(when lsp-sonarlint-java-enabled
       (concat "file://" lsp-sonarlint-java-analyzer-path )))))


(lsp-register-custom-settings
 '(("sonarlint.disableTelemetry" lsp-sonarlint-disable-telemetry)
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
