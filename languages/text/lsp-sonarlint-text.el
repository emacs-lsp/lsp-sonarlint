;;; lsp-sonarlint-text.el --- lsp-sonarlint text module             -*- lexical-binding: t; -*-

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
;; Specific configuration for the SonarLint text plugin.

;; This is NOT and official SonarLint extension.


;;; Code:

(defgroup lsp-sonarlint-text nil
  "lsp-sonarlint text analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-text . "2.0.1"))

(defcustom lsp-sonarlint-text-enabled nil
  "Enable lsp-sonarlint-text plugin."
  :group 'lsp-sonarlint-text
  :type 'boolean)

(defcustom lsp-sonarlint-text-download-url
  "https://repo.maven.apache.org/maven2/org/sonarsource/text/sonar-text-plugin/2.0.1.611/sonar-text-plugin-2.0.1.611.jar"
  "Text plugin download URL."
  :group 'lsp-sonarlint-text
  :type 'string)

(defcustom lsp-sonarlint-text-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-text.jar")
  "Lsp-sonarlint text analyzer location."
  :group 'lsp-sonarlint-text
  :type 'file)

(defvar lsp-sonarlint-text-doc-url "https://www.sonarsource.com/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-text-repository-url "https://github.com/SonarSource/sonar-text"
  "Official sonarlint code extension repository.")





(provide 'lsp-sonarlint-text)
;;; lsp-sonarlint-text.el ends here
