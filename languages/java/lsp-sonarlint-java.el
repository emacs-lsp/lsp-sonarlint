;;; lsp-sonarlint-java.el --- lsp-sonarlint java module             -*- lexical-binding: t; -*-

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
;; Especific configuration for the sonarlint java plugin.

;; This is NOT and official Sonarlint extension.


;;; Code:

(defgroup lsp-sonarlint-java nil
  "lsp-sonarlint java analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-java . "6.4.0"))

(defcustom lsp-sonarlint-java-enabled nil
  "Enable lsp-sonarlint-java plugin."
  :group 'lsp-sonarlint-java
  :type 'boolean)

(defcustom lsp-sonarlint-java-download-url
  "https://binaries.sonarsource.com/Distribution/sonar-java-plugin/sonar-java-plugin-6.4.0.21967.jar"
  "Java plugin download URL."
  :group 'lsp-sonarlint-java
  :type 'string)

(defcustom lsp-sonarlint-java-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-java.jar")
  "Lsp-sonarlint java analyzer location."
  :group 'lsp-sonarlint-java
  :type 'file)

(defvar lsp-sonarlint-java-doc-url "https://www.sonarsource.com/java/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-java-repository-url "https://github.com/SonarSource/sonar-java"
  "Official sonarlint code extension reposiroty.")





(provide 'lsp-sonarlint-java)
;;; lsp-sonarlint-java.el ends here
