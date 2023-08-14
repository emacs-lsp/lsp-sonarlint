;;; lsp-sonarlint-xml.el --- lsp-sonarlint xml module             -*- lexical-binding: t; -*-

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
;; Specific configuration for the SonarLint XML plugin.

;; This is NOT and official SonarLint extension.


;;; Code:

(defgroup lsp-sonarlint-xml nil
  "lsp-sonarlint xml analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-xml . "2.7.0"))

(defcustom lsp-sonarlint-xml-enabled nil
  "Enable lsp-sonarlint-xml plugin."
  :group 'lsp-sonarlint-xml
  :type 'boolean)

(defcustom lsp-sonarlint-xml-download-url
  "https://repo.maven.apache.org/maven2/org/sonarsource/xml/sonar-xml-plugin/2.7.0.3820/sonar-xml-plugin-2.7.0.3820.jar"
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
  "Official sonarlint code extension repository.")


(provide 'lsp-sonarlint-xml)
;;; lsp-sonarlint-xml.el ends here
