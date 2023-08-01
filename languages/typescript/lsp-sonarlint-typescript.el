;;; lsp-sonarlint-typescript.el --- lsp-sonarlint typescript module  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Gueorgui Tcherednitchenko
;; Author: Gueorgui Tcherednitchenko <gt@gueorgui.net>
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
;; Specific configuration for the sonarlint typescript plugin.

;; This is NOT and official SonarLint extension.


;;; Code:

(defgroup lsp-sonarlint-typescript nil
  "lsp-sonarlint typescript analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-typescript . "10.1.0"))

(defcustom lsp-sonarlint-typescript-enabled nil
  "Enable lsp-sonarlint-typescript plugin."
  :group 'lsp-sonarlint-typescript
  :type 'boolean)

(defcustom lsp-sonarlint-typescript-download-url
  "https://repo.maven.apache.org/maven2/org/sonarsource/javascript/sonar-javascript-plugin/10.1.0.21143/sonar-javascript-plugin-10.1.0.21143.jar"
  "Typescript plugin download URL."
  :group 'lsp-sonarlint-typescript
  :type 'string)

(defcustom lsp-sonarlint-typescript-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-javascript.jar") ;; Note: it is the same as for javascript, and that is intentional
  "Lsp-sonarlint typescript analyzer location."
  :group 'lsp-sonarlint-typescript
  :type 'file)

(defvar lsp-sonarlint-typescript-doc-url "https://www.sonarsource.com/ts/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-typescript-repository-url "https://github.com/SonarSource/SonarJS"
  "Official sonarlint code extension repository.")

(provide 'lsp-sonarlint-typescript)
;;; lsp-sonarlint-typescript.el ends here
