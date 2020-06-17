;;; lsp-sonarlint-python.el --- lsp-sonarlint python module             -*- lexical-binding: t; -*-

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
;; Especific configuration for the sonarlint python plugin.

;; This is NOT and official Sonarlint extension.


;;; Code:

(defgroup lsp-sonarlint-python nil
  "lsp-sonarlint python analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-python . "2.9.0"))

(defcustom lsp-sonarlint-python-enabled nil
  "Enable lsp-sonarlint-python plugin."
  :group 'lsp-sonarlint-python
  :type 'boolean)

(defcustom lsp-sonarlint-python-download-url
  "https://binaries.sonarsource.com/Distribution/sonar-python-plugin/sonar-python-plugin-2.9.0.6410.jar"
  "Python plugin download URL."
  :group 'lsp-sonarlint-python
  :type 'string)

(defcustom lsp-sonarlint-python-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-python.jar")
  "Lsp-sonarlint python analyzer location."
  :group 'lsp-sonarlint-python
  :type 'file)

(defvar lsp-sonarlint-python-doc-url "https://www.sonarsource.com/python/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-python-repository-url "https://github.com/SonarSource/sonar-python"
  "Official sonarlint code extension reposiroty.")





(provide 'lsp-sonarlint-python)
;;; lsp-sonarlint-python.el ends here
