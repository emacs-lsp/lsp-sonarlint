;;; lsp-sonarlint-go.el --- lsp-sonarlint Go module             -*- lexical-binding: t; -*-

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
;; Specific configuration for the SonarLint Go plugin.

;; This is NOT and official SonarLint extension.


;;; Code:

(defgroup lsp-sonarlint-go nil
  "lsp-sonarlint Go analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-go . "1.12.0"))

(defcustom lsp-sonarlint-go-enabled nil
  "Enable lsp-sonarlint-go plugin."
  :group 'lsp-sonarlint-go
  :type 'boolean)

(defcustom lsp-sonarlint-go-download-url
  "https://repo.maven.apache.org/maven2/org/sonarsource/slang/sonar-go-plugin/1.12.0.4259/sonar-go-plugin-1.12.0.4259.jar"
  "Go plugin download URL."
  :group 'lsp-sonarlint-go
  :type 'string)

(defcustom lsp-sonarlint-go-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-go.jar")
  "Lsp-sonarlint Go analyzer location."
  :group 'lsp-sonarlint-go
  :type 'file)

(defvar lsp-sonarlint-go-doc-url "https://www.sonarsource.com/go/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-go-repository-url "https://github.com/SonarSource/slang/"
  "Official sonarlint code extension repository.")


(provide 'lsp-sonarlint-go)
;;; lsp-sonarlint-go.el ends here
