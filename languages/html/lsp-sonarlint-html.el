;;; lsp-sonarlint-html.el --- lsp-sonarlint html module             -*- lexical-binding: t; -*-

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
;; Especific configuration for the sonarlint html plugin.

;; This is NOT and official Sonarlint extension.


;;; Code:

(defgroup lsp-sonarlint-html nil
  "lsp-sonarlint html analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-html . "3.2.0"))

(defcustom lsp-sonarlint-html-enabled nil
  "Enable lsp-sonarlint-html plugin."
  :group 'lsp-sonarlint-html
  :type 'boolean)

(defcustom lsp-sonarlint-html-download-url
  "https://binaries.sonarsource.com/Distribution/sonar-html-plugin/sonar-html-plugin-3.2.0.2082.jar"
  "Html plugin download URL."
  :group 'lsp-sonarlint-html
  :type 'string)

(defcustom lsp-sonarlint-html-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-html.jar")
  "Lsp-sonarlint html analyzer location."
  :group 'lsp-sonarlint-html
  :type 'file)

(defvar lsp-sonarlint-html-doc-url "https://www.sonarsource.com/html/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-html-repository-url "https://github.com/SonarSource/sonar-html"
  "Official sonarlint code extension reposiroty.")





(provide 'lsp-sonarlint-html)
;;; lsp-sonarlint-html.el ends here
