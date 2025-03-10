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
(require 'cl-lib)
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

(defun lsp-sonarlint-test--overlay-strings (prop-name loc-fun)
  "Return a list of plists with PROP-NAME and their locs obtained with LOC-FUN."
  (cl-remove nil (mapcar (lambda (ovl) (if-let ((str-before (overlay-get ovl prop-name)))
                                      `(,prop-name ,(substring-no-properties str-before)
                                        :pos ,(funcall loc-fun ovl))
                                    nil))
                         (overlays-in (point-min) (point-max)))))

(defun lsp-sonarlint-test--buf-string-with-overlay-strings ()
  "Contents of current buffer interspersed with overlay-attached strings."
  (let* ((pieces '())
         (last-pos (point-min))
         (before-strings (lsp-sonarlint-test--overlay-strings
                          'before-string #'overlay-start))
         (after-strings (lsp-sonarlint-test--overlay-strings
                         'after-string #'overlay-end))
         (all-strings (sort (append before-strings after-strings)
                            (lambda (str1 str2)
                              (let ((pos1 (plist-get str1 :pos))
                                    (pos2 (plist-get str2 :pos)))
                                (or (< pos1 pos2)
                                    ;; before-string is inserted before after-string
                                    (and (= pos1 pos2)
                                         (plist-member str1 'after-string)
                                         (not (plist-member str2 'after-string)))))))))
    (dolist (str all-strings)
      (let ((pos (plist-get str :pos)))
        (push (buffer-substring-no-properties last-pos pos) pieces)
        (when-let ((after-string (plist-get str 'after-string)))
          (push after-string pieces))
        (when-let ((before-string (plist-get str 'before-string)))
          (push before-string pieces))
        (setq last-pos pos)))
    (concat (string-join (nreverse pieces))
            (buffer-substring-no-properties last-pos (point-max)))))

(defun lsp-sonarlint-test--place-overlay (line marker)
  "Add overlay at LINE covering chars pointed to by MARKER in current buffer."
  (let* ((range (lsp-sonarlint-test-range-make (buffer-string) line marker))
         (line (plist-get range :line))
         (from (plist-get range :from))
         (to (plist-get range :to)))
    (save-excursion
      (goto-char (point-min))
      (forward-line (- line 1))
      (make-overlay (+ from (line-beginning-position))
                    (+ to (line-beginning-position))
                    (current-buffer)))))

(ert-deftest lsp-sonarlint-overlay-strings ()
  "Test `lsp-sonarlint-test--buf-string-with-overlay-strings' on corner cases"
  (with-temp-buffer
    (unwind-protect
        (progn
          (insert "
Some string here
")
          (overlay-put (lsp-sonarlint-test--place-overlay
                        "Some string here"
                        "^^^^            ")
                       'before-string
                       "Heho")
          (overlay-put (lsp-sonarlint-test--place-overlay
                        "Some string here"
                        "^^^^            ")
                       'after-string
                       "after")
          (overlay-put (lsp-sonarlint-test--place-overlay
                        "Some string here"
                        "    ^           ")
                       'before-string
                       "bef1")
          (overlay-put (lsp-sonarlint-test--place-overlay
                        "Some string here"
                        "    ^           ")
                       'after-string
                       "aft1")
          (overlay-put (lsp-sonarlint-test--place-overlay
                        "Some string here"
                        "            ^^^^")
                       'after-string
                       "
next str")
          (should (equal (lsp-sonarlint-test--buf-string-with-overlay-strings)
                         "
HehoSomeafterbef1 aft1string here
next str
"))
          )
      (remove-overlays))))

(ert-deftest lsp-sonarlint-display-secondary-messages ()
  "Test that secondary locations are displayed correctly."
  (let ((target-file-buf (find-file-noselect lsp-sonarlint-test--file-path))
        (lsp-sonarlint--scale-inline-msg-offset nil))
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
        (lsp-sonarlint--show-all-locations command)))
    (with-current-buffer lsp-sonarlint--secondary-messages-buffer-name
      (should (equal (lsp-sonarlint-test--buf-string-with-overlay-strings)
                     "Redundant branching
1Identical code
2Identical code")))
    (with-current-buffer target-file-buf
      (should (equal (lsp-sonarlint-test--buf-string-with-overlay-strings)
                     "
int divide_seventeen(int param) {
  Redundant branching
  if (param == 0) {
     Identical code
    1int a = 0;
  } else {
     Identical code
    2int b = 0;
  }
  return 10 / param;
}
")))))

(defun lsp-sonarlint-test--select-message (msg)
  "Select the step with message MSG in the SonarLint secondary messages buffer."
  (with-current-buffer lsp-sonarlint--secondary-messages-buffer-name
      (goto-char (point-min))
      (search-forward msg)
      ;; Deliberately using interactive functions here to trigger line-move
      ;; hooks
      (previous-line)
      (forward-char -1) ; Make sure to stay within found string to hit the overlay
      (next-line)))

(ert-deftest lsp-sonarlint-navigate-to-sec-location ()
  "Test that point moves to locations of selected messages."
  (let ((target-file-buf (find-file-noselect lsp-sonarlint-test--file-path))
        (lsp-sonarlint--scale-inline-msg-offset nil))
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
             (sec-flow1 `((:message "Code A" :range ,secondary-range1)))
             (secondary-range2
              (lsp-sonarlint-test-range-make (buffer-string)
                                             "    int b = 0;"
                                             "    ^^^^^^^^^^"))
             (sec-flow2 `((:message "Code B" :range ,secondary-range2)))
             (command (lsp-sonarlint-test--secloc-command
                       primary-loc (list sec-flow1 sec-flow2))))
        (lsp-sonarlint--show-all-locations command)))
    (with-current-buffer target-file-buf
      (goto-char (point-min)))
    (lsp-sonarlint-test--select-message "Code A")
    (with-current-buffer target-file-buf
      (should (equal (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position))
                     "    int a = 0;")))
    (lsp-sonarlint-test--select-message "Code B")
    (with-current-buffer target-file-buf
      (should (equal (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position))
                     "    int b = 0;")))))

(ert-deftest lsp-sonarlint-display-execution-flow ()
  "Test that flow steps are displayed correctly and in order."
  (let ((target-file-buf (find-file-noselect lsp-sonarlint-test--file-path))
        (lsp-sonarlint--scale-inline-msg-offset nil))
    (with-current-buffer target-file-buf
      (let* ((primary-range (lsp-sonarlint-test-range-make
                             (buffer-string)
                             "  return 10 / param;"
                             "            ^       "))
             (primary-loc `(:message "Division by 0" :range ,primary-range))
             (flow
              ;; SonarLint sends flow in reverse order
              (list
               ;; SonarLint often duplicates primary message in a flow step
               primary-loc
               `(:message "Assigning a 0"
                 :range
                 ,(lsp-sonarlint-test-range-make (buffer-string)
                                                 "    int a = 0;"
                                                 "        ^^^^^ "))
               `(:message "Taking true branch"
                 :range
                 ,(lsp-sonarlint-test-range-make (buffer-string)
                                                 "  if (param == 0) {"
                                                 "  ^^               "))
               `(:message "Assuming param is 0"
                 :range
                 ,(lsp-sonarlint-test-range-make (buffer-string)
                                                 "  if (param == 0) {"
                                                 "            ^^^^   "))
               `(:message "Evaluating condition"
                 :range
                 ,(lsp-sonarlint-test-range-make (buffer-string)
                                                 "  if (param == 0) {"
                                                 "      ^^^^^^^^^^   "))))
             (command (lsp-sonarlint-test--secloc-command
                       primary-loc (list flow))))
        (lsp-sonarlint--show-all-locations command)))
    (with-current-buffer lsp-sonarlint--secondary-messages-buffer-name
      (should (equal (lsp-sonarlint-test--buf-string-with-overlay-strings)
                     "Division by 0
1Evaluating condition
2Assuming param is 0
3Taking true branch
4Assigning a 0
5Division by 0")))
    (with-current-buffer target-file-buf
      ;; In-line messages appear shifted here because they are rendered
      ;; with the same font.
      ;; In the actual buffer these strings have smaller font, so
      ;; they start closer to the left.
      (should (equal (lsp-sonarlint-test--buf-string-with-overlay-strings)
                     "
int divide_seventeen(int param) {
   Taking true branch
        Evaluating condition
  3if (1param 2== 0) {
               Assuming param is 0
         Assigning a 0
    int 4a = 0;
  } else {
    int b = 0;
  }
             Division by 0
  return 10 5/ param;
}
")))))

(ert-deftest lsp-sonarlint-add-inline-messages-deduplicate ()
  "`lsp-sonarlint--add-inline-messages' deduplicates messages."
  (with-temp-buffer
    (insert "
Some long line with words clearly separated
")
    (let ((locations
           (list
            `(:message "first"
              :overlay ,(lsp-sonarlint-test--place-overlay
                         "Some long line with words clearly separated"
                         "^^^^                                       "))
            `(:message "first"
              :overlay ,(lsp-sonarlint-test--place-overlay
                         "Some long line with words clearly separated"
                         "^^^^                                       "))))
          (lsp-sonarlint--scale-inline-msg-offset nil))
      (unwind-protect
          (progn
            (lsp-sonarlint--add-inline-messages locations)
            (should (equal (lsp-sonarlint-test--buf-string-with-overlay-strings)
                           "
first
Some long line with words clearly separated
")))
          (remove-overlays)))))

(ert-deftest lsp-sonarlint-add-inline-messages-both-sides ()
  "`lsp-sonarlint--add-inline-messages' places messages on both sides of a line."
  (with-temp-buffer
    (insert "
Some long line with words clearly separated
")
    (let ((locations
           (list
            `(:message "first"
              :overlay ,(lsp-sonarlint-test--place-overlay
                         "Some long line with words clearly separated"
                         "^^^^                                       "))
            `(:message "second"
              :overlay ,(lsp-sonarlint-test--place-overlay
                         "Some long line with words clearly separated"
                         "^^^^                                       "))))
          (lsp-sonarlint--scale-inline-msg-offset nil))
      (unwind-protect
          (progn
            (lsp-sonarlint--add-inline-messages locations)
            (should (equal (lsp-sonarlint-test--buf-string-with-overlay-strings)
                           "
second
Some long line with words clearly separated
first
")))
          (remove-overlays)))))

(ert-deftest lsp-sonarlint-add-inline-messages-combine ()
  "`lsp-sonarlint--add-inline-messages' combines messages pairwise."
  (with-temp-buffer
    (insert "
Some long line with words clearly separated
")
    (let ((locations
           (list
            `(:message "first"
              :overlay ,(lsp-sonarlint-test--place-overlay
                         "Some long line with words clearly separated"
                         "^^^^                                       "))
            `(:message "second"
              :overlay ,(lsp-sonarlint-test--place-overlay
                         "Some long line with words clearly separated"
                         "                          ^^^^^^^          "))
            `(:message "third"
              :overlay ,(lsp-sonarlint-test--place-overlay
                         "Some long line with words clearly separated"
                         "          ^^^^                             "))
            `(:message "fourth"
              :overlay ,(lsp-sonarlint-test--place-overlay
                         "Some long line with words clearly separated"
                         "                                  ^        "))))
          (lsp-sonarlint--scale-inline-msg-offset nil))
      (unwind-protect
          (progn
            (lsp-sonarlint--add-inline-messages locations)
            (should (equal (lsp-sonarlint-test--buf-string-with-overlay-strings)
                           "
first                             fourth
Some long line with words clearly separated
          third           second
")))
          (remove-overlays)))))


;;; lsp-sonarlint-secondar-locations-test.el ends here
