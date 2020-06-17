;;; lsp-sonarlint-javascript.el --- lsp-sonarlint javascript module             -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Fermin Munoz
;; URL: https://gitlab.com/sasanidas/lsp-sonarlint
;; Keywords: languages

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
;; Especific configuration for the sonarlint javascript plugin.

;; This is NOT and official Sonarlint extension.


;;; Code:

(defgroup lsp-sonarlint-javascript nil
  "lsp-sonarlint javascript analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-javascript . "6.2.1"))

(defcustom lsp-sonarlint-javascript-enabled nil
  "Enable lsp-sonarlint-javascript plugin."
  :group 'lsp-sonarlint-javascript
  :type 'boolean)

(defcustom lsp-sonarlint-javascript-download-url
  "https://binaries.sonarsource.com/Distribution/sonar-javascript-plugin/sonar-javascript-plugin-6.2.1.12157.jar"
  "Javascript plugin download URL."
  :group 'lsp-sonarlint-javascript
  :type 'string)

(defcustom lsp-sonarlint-javascript-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-javascript.jar")
  "Lsp-sonarlint javascript analyzer location."
  :group 'lsp-sonarlint-javascript
  :type 'file)

(defvar lsp-sonarlint-javascript-doc-url "https://www.sonarsource.com/js/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-javascript-repository-url "https://github.com/SonarSource/SonarJS"
  "Official sonarlint code extension reposiroty.")





(provide 'lsp-sonarlint-javascript)
;;; lsp-sonarlint-javascript.el ends here
