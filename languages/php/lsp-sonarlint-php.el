;;; lsp-sonarlint-php.el --- lsp-sonarlint php module             -*- lexical-binding: t; -*-

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
;; Especific configuration for the sonarlint php plugin.

;; This is NOT and official Sonarlint extension.


;;; Code:

(defgroup lsp-sonarlint-php nil
  "lsp-sonarlint php analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-php . "3.5.0"))

(defcustom lsp-sonarlint-php-enabled nil
  "Enable lsp-sonarlint-php plugin."
  :group 'lsp-sonarlint-php
  :type 'boolean)

(defcustom lsp-sonarlint-php-download-url
  "https://binaries.sonarsource.com/Distribution/sonar-php-plugin/sonar-php-plugin-3.5.0.5655.jar"
  "Php plugin download URL."
  :group 'lsp-sonarlint-php
  :type 'string)

(defcustom lsp-sonarlint-php-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-php.jar")
  "Lsp-sonarlint php analyzer location."
  :group 'lsp-sonarlint-php
  :type 'file)

(defvar lsp-sonarlint-php-doc-url "https://www.sonarsource.com/php/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-php-repository-url "https://github.com/SonarSource/sonar-php"
  "Official sonarlint code extension reposiroty.")





(provide 'lsp-sonarlint-php)
;;; lsp-sonarlint-php.el ends here
