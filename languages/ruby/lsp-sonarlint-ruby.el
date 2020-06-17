;;; lsp-sonarlint-ruby.el --- lsp-sonarlint ruby module             -*- lexical-binding: t; -*-

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
;; Especific configuration for the sonarlint ruby plugin.

;; This is NOT and official Sonarlint extension.


;;; Code:

(defgroup lsp-sonarlint-ruby nil
  "lsp-sonarlint ruby analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-ruby . "1.7.0"))

(defcustom lsp-sonarlint-ruby-enabled nil
  "Enable lsp-sonarlint-ruby plugin."
  :group 'lsp-sonarlint-ruby
  :type 'boolean)

(defcustom lsp-sonarlint-ruby-download-url
  "https://binaries.sonarsource.com/Distribution/sonar-ruby-plugin/sonar-ruby-plugin-1.7.0.883.jar"
  "Ruby plugin download URL."
  :group 'lsp-sonarlint-ruby
  :type 'string)

(defcustom lsp-sonarlint-ruby-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-ruby.jar")
"Lsp-sonarlint ruby analyzer location."
  :group 'lsp-sonarlint-ruby
  :type 'file)

(defvar lsp-sonarlint-ruby-doc-url "https://www.sonarsource.com/ruby/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-ruby-repository-url "https://github.com/SonarSource/slang/"
  "Official sonarlint code extension reposiroty.")





(provide 'lsp-sonarlint-ruby)
;;; lsp-sonarlint-ruby.el ends here
