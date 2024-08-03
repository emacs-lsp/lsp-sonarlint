;;; lsp-sonarlint-secondar-locations-test.el --- Secondary locations tests for Sonarlint LSP client   -*- lexical-binding: t; -*-
;;;
;; Author: Arseniy Zaostrovnykh
;; Created: 02 August 2024
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
;; Tests for the display of secondary locations and flow steps for SonarLint issues.

;;; Code:

(require 'lsp-mode)
(require 'lsp-sonarlint)
(load-file (expand-file-name "lsp-sonarlint-test-utils.el"
                             (file-name-directory (or load-file-name (buffer-file-name)))))

(defvar lsp-sonarlint-test--file-path (lsp-sonarlint-sample-file "secondaries.txt"))
(defvar lsp-sonarlint-test--file-uri (concat "file://" lsp-sonarlint-test--file-path))

(defun lsp-sonarlint-test--find-line (file-content line)
  "Find LINE in the multi-line FILE-CONTENT string."
  (let ((lines (split-string file-content "\n"))
        (line-number 1)
        (found nil))
    (while (and lines (not found))
      (when (string= (car lines) line)
        (setq found line-number))
      (setq lines (cdr lines))
      (setq line-number (1+ line-number)))
    (when (not found)
      (error "Line %s not found" line))
    found))


(defun lsp-sonarlint-test-range-make (file-content line marker)
  "Create a single-line diagnostics range summary.

Find LINE in FILE-CONTENT and take that as the line number.
Set the :from and :to characters to reflect the position of
`^^^^' in the MARKER.

Example (suppose line #3 of current buffer is \"full line\"):

(lsp-test-range-make (buffer-string)
                     \"full line\"
                     \"     ^^^^\")

-> (:line 3 :from 5 :to 8)
"
  (let ((line-number (lsp-sonarlint-test--find-line file-content line)))
    (should-not (null line-number))
    (should (eq (length marker) (length line)))
    (should (string-match "^ *\\(\\^+\\) *$" marker))
    (list :line line-number :from (match-beginning 1) :to (match-end 1))))

(defun lsp-sonarlint-test--line-range->ht (range)
  "Convert RANGE to a hash table."
  (lsp-ht ("startLine" (plist-get range :line))
          ("startLineOffset" (plist-get range :from))
          ("endLine" (plist-get range :line))
          ("endLineOffset" (plist-get range :to))
          ("hash" "")))

(defun lsp-sonarlint-test--secloc-command (primary secondary-flows)
  "Command for a SonarLint issue with PRIMARY location and SECONDARY-FLOWS.

A location is a plist with message and line range as follows:
(:message \"Identical code\" :range (:line 29 :from 4 :to 14))
PRIMARY is a location. SECONDARY-FLOWS is a list of lists of
locations, each list representing a flow.

Returns a hashtable representing the command as received from
SonarLint LSP server."
  (lsp-ht ("title" "Show all locations for issue 'cpp:S3923'")
          ("command" "SonarLint.ShowAllLocations")
          ("arguments"
           (vector
            (lsp-ht ("fileUri" lsp-sonarlint-test--file-uri)
                    ("message" (plist-get primary :message))
                    ("shouldOpenRuleDescription" t)
                    ("severity" "MAJOR")
                    ("ruleKey" "cpp:S3923")
                    ("flows"
                     (apply
                      #'vector
                      (mapcar
                       (lambda (flow)
                         (lsp-ht
                          ("locations"
                           (apply
                            #'vector
                            (mapcar (lambda (loc)
                                      (lsp-ht
                                       ("textRange"
                                        (lsp-sonarlint-test--line-range->ht
                                         (plist-get loc :range)))
                                       ("uri" lsp-sonarlint-test--file-uri)
                                       ("filePath" lsp-sonarlint-test--file-path)
                                       ("message" (plist-get loc :message))
                                       ("exists" t)
                                       ("codeMatches" t)))
                                    flow)))))
                       secondary-flows)))
                    ("textRange"
                     (lsp-sonarlint-test--line-range->ht (plist-get primary :range)))
                    ("codeMatches" nil))))))

(ert-deftest lsp-sonarlint-test--display-secondary-messages ()
  "Test that secondary locations are displayed correctly."
  (let ((target-file-buf (find-file-noselect lsp-sonarlint-test--file-path)))
    (with-current-buffer target-file-buf
      (let* ((primary-range (lsp-sonarlint-test-range-make
                             (buffer-string)
                             "  if (param == 0) {"
                             "  ^^               "))
             (primary-loc `(:message "Redundant branching" :range ,primary-range))
             (secondary-range1
              (lsp-sonarlint-test-range-make (buffer-string)
                                             "    int a = 0;"
                                             "    ^^^^^^^^^^"))
             (sec-flow1 `((:message "Identical code" :range ,secondary-range1)))
             (secondary-range2
              (lsp-sonarlint-test-range-make (buffer-string)
                                             "    int b = 0;"
                                             "    ^^^^^^^^^^"))
             (sec-flow2 `((:message "Identical code" :range ,secondary-range2)))
             (command (lsp-sonarlint-test--secloc-command
                       primary-loc (list sec-flow1 sec-flow2))))
        (lsp-sonarlint--show-all-locations command)
        (with-current-buffer lsp-sonarlint--secondary-messages-buffer-name
          (should (equal (buffer-string)
                         "Redundant branching
Identical code
Identical code")))))))


;;; lsp-sonarlint-secondar-locations-test.el ends here
