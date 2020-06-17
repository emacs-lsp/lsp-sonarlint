;;; lsp-sonarlint-php.el --- lsp-sonarlint scala module             -*- lexical-binding: t; -*-

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
;; Especific configuration for the sonarlint scala plugin.

;; This is NOT and official Sonarlint extension.


;;; Code:

(defgroup lsp-sonarlint-scala nil
  "lsp-sonarlint scala analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-scala . "1.7.0"))

(defcustom lsp-sonarlint-scala-enabled nil
  "Enable lsp-sonarlint-scala plugin."
  :group 'lsp-sonarlint-scala
  :type 'boolean)

(defcustom lsp-sonarlint-scala-download-url
  "https://binaries.sonarsource.com/Distribution/sonar-scala-plugin/sonar-scala-plugin-1.7.0.883.jar"
  "Scala plugin download URL."
  :group 'lsp-sonarlint-scala
  :type 'string)

(defcustom lsp-sonarlint-scala-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-scala.jar")
  "Lsp-sonarlint scala analyzer location."
  :group 'lsp-sonarlint-scala
  :type 'file)

(defvar lsp-sonarlint-scala-doc-url "https://www.sonarsource.com/scala/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-scala-repository-url "https://github.com/SonarSource/slang/"
  "Official sonarlint code extension reposiroty.")





(provide 'lsp-sonarlint-scala)
;;; lsp-sonarlint-scala.el ends here
