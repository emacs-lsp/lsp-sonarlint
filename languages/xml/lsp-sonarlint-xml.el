;;; lsp-sonarlint-php.el --- lsp-sonarlint xml module             -*- lexical-binding: t; -*-

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
;; Especific configuration for the sonarlint xml plugin.

;; This is NOT and official Sonarlint extension.


;;; Code:

(defgroup lsp-sonarlint-xml nil
  "lsp-sonarlint xml analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-xml . "2.0.1"))

(defcustom lsp-sonarlint-xml-enabled nil
  "Enable lsp-sonarlint-xml plugin."
  :group 'lsp-sonarlint-xml
  :type 'boolean)

(defcustom lsp-sonarlint-xml-download-url
  "https://binaries.sonarsource.com/Distribution/sonar-xml-plugin/sonar-xml-plugin-2.0.1.2020.jar"
  "Xml plugin download URL."
  :group 'lsp-sonarlint-xml
  :type 'string)

(defcustom lsp-sonarlint-xml-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-xml.jar")
  "Lsp-sonarlint xml analyzer location."
  :group 'lsp-sonarlint-xml
  :type 'file)

(defvar lsp-sonarlint-xml-doc-url "https://www.sonarsource.com/xml/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-xml-repository-url "https://github.com/SonarSource/sonar-xml"
  "Official sonarlint code extension reposiroty.")





(provide 'lsp-sonarlint-xml)
;;; lsp-sonarlint-xml.el ends here
