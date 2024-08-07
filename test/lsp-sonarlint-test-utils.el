;;; integration.el --- Integration tests for Sonarlint LSP client   -*- lexical-binding: t; -*-
;;;
;; Author: Arseniy Zaostrovnykh
;; Created: 11 July 2023
;; License: GPL-3.0-or-later
;;
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
;; Utility functions for the LSP SonarLint tests.

;;; Code:

(defun lsp-sonarlint-fixtures-dir ()
  "Directory of the test fixtures for these tests."
  (concat
   (file-name-directory
    (directory-file-name (file-name-directory (symbol-file #'lsp-sonarlint-fixtures-dir))))
   "fixtures/"))

(defun lsp-sonarlint-sample-file (fname)
  "Get the full path of the sample file FNAME."
  (concat (lsp-sonarlint-fixtures-dir) fname))

(provide 'lsp-sonarlint-test-utils)

;;; lsp-sonarlint-test-utils.el ends here
