;;; lsp-sonarlint-cpp-test.el --- CFamily analyzer-specific tests for Sonarlint LSP client   -*- lexical-binding: t; -*-
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
;; Tests for the C++-specific integration of the LSP mode and SonarLint language server.

;;; Code:

(require 'lsp-mode)
(require 'lsp-sonarlint)
(load-file (expand-file-name "lsp-sonarlint-test-utils.el"
                             (file-name-directory (or load-file-name (buffer-file-name)))))

(defun lsp-sonarlint--get-compdb-for-sample-file (cpp-file)
  "Relative to fixtures folder, get compile_commands.json for CPP-FILE."
  (let ((buf (find-file-noselect (lsp-sonarlint-sample-file cpp-file))))
    (with-current-buffer buf
     (file-relative-name (lsp-sonarlint--get-compile-commands)
                         (lsp-sonarlint-fixtures-dir)))))

(ert-deftest lsp-sonarlint--get-compile-commands-test ()
  (should (equal (lsp-sonarlint--get-compdb-for-sample-file "compdb/dir1/1.cpp")
                 "compdb/dir1/compile_commands.json"))
  (should (equal (lsp-sonarlint--get-compdb-for-sample-file "compdb/dir1/dir11/11.cpp")
                 "compdb/dir1/dir11/compile_commands.json"))
  (should (equal (lsp-sonarlint--get-compdb-for-sample-file "compdb/dir1/dir12/12.cpp")
                 "compdb/dir1/compile_commands.json"))
  (should (equal (lsp-sonarlint--get-compdb-for-sample-file "compdb/dir1/dir12/dir121/121.cpp")
                 "compdb/dir1/compile_commands.json")))

(ert-deftest lsp-sonarlint--get-compile-commands-interactive-test ()
  (cl-letf (((symbol-function 'read-file-name) (lambda (_prompt)
                                                 (concat (lsp-sonarlint-fixtures-dir) "my_db.json"))))
  (should (equal (lsp-sonarlint--get-compdb-for-sample-file "compdb/no-compdb/no-compdb.cpp")
                 "my_db.json"))))

;;; lsp-sonarlint-cpp-test.el ends here
