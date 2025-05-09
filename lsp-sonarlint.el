;;; lsp-sonarlint.el --- Emacs SonarLint lsp client              -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Fermin Munoz

;; Author: Fermin MF <fmfs@posteo.net>
;; Created: 13 Jun 2020
;; Version: 0.0.1
;; Keywords: languages, tools, php, javascript, typescript, go, xml, html, java, python
;; URL: https://github.com/emacs-lsp/lsp-sonarlint
;; Package-Requires: ((emacs "28.1") (dash "2.12.0") (lsp-mode "6.3") (ht "2.3"))
;; License: GPL-3.0-or-later

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
;; SonarLint LSP extensions for GNU Emacs, add support for the majority of sonarlint languages
;; including: php, javascript, typescript, html, python and java.

;; This is NOT an official SonarLint extension.


;;; Code:

(require 'lsp-mode)
(require 'dash)
(require 'cus-edit)
(require 'ht)
(require 'shr)
(require 'hl-line)

(defgroup lsp-sonarlint nil
  "SonarLint lsp server group"
  :group 'lsp-mode
  :link '(url-link "https://github.com/emacs-lsp/lsp-sonarlint")
  :package-version '(lsp-mode . "6.4"))

(defcustom lsp-sonarlint-download-url
  (format "https://github.com/SonarSource/sonarlint-vscode/releases/download/4.6.0%%2B76435/sonarlint-vscode-%s-4.6.0.vsix"
          (pcase system-type
            ('gnu/linux "linux-x64")
            ('darwin
             (pcase (lsp-resolve-value lsp--system-arch)
               ('x64     "darwin-x64")
               ('arm64   "darwin-arm64")))
            ('windows-nt "win32-x64")))
  "Official SonarLint VSCode extension (vsix).
It contains the necessary language server and analyzers."
  :type 'string)

(defcustom lsp-sonarlint-download-dir
  (concat (file-name-directory load-file-name) "sonarlint-vscode")
  "Location where VSCode's sonarlint extension should be found.
If absent, it will be downloaded from github and unzipped there."
  :group 'lsp-sonarlint
  :type 'directory)

(defcustom lsp-sonarlint-modes-enabled '(php-mode
                                         go-mode
                                         html-mode
                                         web-mode
                                         js-mode
                                         js2-mode
                                         rjsx-mode
                                         typescript-mode
                                         typescript-tsx-mode
                                         python-mode
                                         c-mode
                                         c++-mode
                                         c-or-c++-mode
                                         c-ts-mode
                                         c++-ts-mode
                                         c-or-c++-ts-mode
                                         java-mode
                                         xml-mode
                                         nxml-mode)
  "List of major modes that enable SonarLint backend for LSP mode."
  :group 'lsp-sonarlint
  :type 'file)

(defcustom lsp-sonarlint-disable-telemetry t
  "Disable sending anonymous usage statistics to SonarSource.
To see a sample of the data that are collected
https://github.com/SonarSource/sonarlint-vscode/blob/master/telemetry-sample.md."
  :group 'lsp-sonarlint
  :type 'boolean)

(defcustom lsp-sonarlint-test-file-pattern "{**/test/**,**/*test*,**/*Test*}"
  "Files whose name match java global are considered as test files by analyzers.
Most rules are not evaluated on test files.
Example: `{**/test/**,**/*test*,**/*Test*}`"
  :group 'lsp-sonarlint
  :type 'string)

(defcustom lsp-sonarlint-show-analyzer-logs nil
  "Show analyzer's logs in the SonarLint output."
  :group 'lsp-sonarlint
  :type 'boolean)

(defcustom lsp-sonarlint-verbose-logs nil
  "Enable verbose logging of the SonarLint language server."
  :group 'lsp-sonarlint
  :type 'boolean)

(defcustom lsp-sonarlint-vmargs ""
  "Extra JVM arguments used to launch the SonarLint LSP.
e.g. `-Xmx1024m`."
  :group 'lsp-sonarlint
  :type 'string)

(defcustom lsp-sonarlint-cfamily-compile-commands-path
  "${workspaceFolder}/compile_commands.json"
  "Location of the compile_commands.json file.
It is needed in C/C++ to provide your compilation options."
  :group 'lsp-sonarlint
  :type 'file)

(defconst lsp-sonarlint-go-doc-url "https://www.sonarsource.com/go/"
  "Documentation sonarsource URL.")

(defconst lsp-sonarlint-go-repository-url "https://github.com/SonarSource/slang/"
  "Official sonarlint code extension repository.")

(defconst lsp-sonarlint-html-doc-url "https://www.sonarsource.com/html/"
  "Documentation sonarsource URL.")

(defconst lsp-sonarlint-html-repository-url "https://github.com/SonarSource/sonar-html"
  "Official sonarlint code extension repository.")

(defconst lsp-sonarlint-java-doc-url "https://www.sonarsource.com/java/"
  "Documentation sonarsource URL.")

(defconst lsp-sonarlint-java-repository-url "https://github.com/SonarSource/sonar-java"
  "Official sonarlint code extension repository.")

(defconst lsp-sonarlint-c-doc-url "https://www.sonarsource.com/c"
  "Documentation sonarsource URL.")

(defconst lsp-sonarlint-c++-doc-url "https://www.sonarsource.com/cpp"
  "Documentation sonarsource URL.")

(defconst lsp-sonarlint-javascript-doc-url "https://www.sonarsource.com/js/"
  "Documentation sonarsource URL.")

(defconst lsp-sonarlint-javascript-repository-url "https://github.com/SonarSource/SonarJS"
  "Official sonarlint code extension repository.")

(defconst lsp-sonarlint-php-doc-url "https://www.sonarsource.com/php/"
  "Documentation sonarsource URL.")

(defconst lsp-sonarlint-php-repository-url "https://github.com/SonarSource/sonar-php"
  "Official sonarlint code extension repository.")

(defconst lsp-sonarlint-python-doc-url "https://www.sonarsource.com/python/"
  "Documentation sonarsource URL.")

(defconst lsp-sonarlint-python-repository-url "https://github.com/SonarSource/sonar-python"
  "Official sonarlint code extension repository.")

(defconst lsp-sonarlint-text-doc-url "https://www.sonarsource.com/"
  "Documentation sonarsource URL.")

(defconst lsp-sonarlint-text-repository-url "https://github.com/SonarSource/sonar-text"
  "Official sonarlint code extension repository.")

(defconst lsp-sonarlint-typescript-doc-url "https://www.sonarsource.com/ts/"
  "Documentation sonarsource URL.")

(defconst lsp-sonarlint-typescript-repository-url "https://github.com/SonarSource/SonarJS"
  "Official sonarlint code extension repository.")

(defconst lsp-sonarlint-xml-doc-url "https://www.sonarsource.com/xml/"
  "Documentation sonarsource URL.")

(defconst lsp-sonarlint-xml-repository-url "https://github.com/SonarSource/sonar-xml"
  "Official sonarlint code extension repository.")

(defun lsp-sonarlint-download()
  "Download the VSCode extension and unzips it.

Follows the customizable variables : `lsp-sonarlint-download-url' and
`lsp-sonarlint-download-dir'"
  (interactive)
  (let* ((dest-dir (file-name-directory lsp-sonarlint-download-dir))
         (vsix-path (concat dest-dir "sonarlint.zip")))
    (unless (file-exists-p lsp-sonarlint-download-dir)
      (unless (file-exists-p vsix-path)
        (url-copy-file lsp-sonarlint-download-url vsix-path))
      (lsp-unzip vsix-path lsp-sonarlint-download-dir)
      (if (file-exists-p lsp-sonarlint-download-dir)
          (progn
            (delete-file vsix-path)
            (message "Sonarlint : successfully downloaded to %s" lsp-sonarlint-download-dir))

        ;; For unzip issues on windows, see https://github.com/emacs-lsp/lsp-mode/issues/3022
        (warn "Could not unzip the VSCode extension, Either :\n\
- Set `lsp-unzip-script' properly : on windows set it to `lsp-ext-pwsh-script'\n\
- Download manually `lsp-sonarlint-download-url', unzip it, move it to `lsp-sonarlint-download-dir'")))))

(defcustom lsp-sonarlint-auto-download
  nil
  "If non nil, the analyzers will be downloaded at startup if needed.
See also `lsp-sonarlint-download-dir' and `lsp-sonarlint-download-url'."
  :group 'lsp-sonarlint
  :set (lambda (sym auto-download) ; Trigger download when set to non-nil
         (set sym auto-download)
	     (when auto-download
           (lsp-sonarlint-download)))
  :group 'lsp-sonarlint
  :type 'boolean)

(defcustom lsp-sonarlint-use-system-jre
  nil
  "Use the system java runtime instead of the one bundled in VSCode's extension."
  :group 'lsp-sonarlint
  :type 'boolean)

(defcustom lsp-sonarlint-enabled-analyzers
  'all
  "Specify which analyzers you want to enable.
- Use the \\='all symbol to enable all analyzers.
- Use a list of analyzer names if you only need a subset.  In this case,
use `lsp-sonarlint-available-analyzers' to list available analyzers."
  :group 'lsp-sonarlint
  :type '(choice (const :tag "All analyzers" all)
                 (const :tag "No analyzer" nil)
                 (repeat :tag "A subset (see `lsp-sonarlint-available-analyzers')" string)))

(defun lsp-sonarlint--extract-analyzer-name(analyzer-path)
  "Extracts the analyzer name from ANALYZER-PATH"
  (replace-regexp-in-string "sonar\\(lint\\)?" "" (file-name-base analyzer-path)))

(defun lsp-sonarlint-available-analyzers()
  "Return a list of the available analyzers."
  (interactive)
  (let* ((root-dir lsp-sonarlint-download-dir)
         (plugin-dir (concat root-dir "/extension/analyzers"))
         (plugin-paths (directory-files plugin-dir t ".jar"))
         (plugin-names (mapcar #'lsp-sonarlint--extract-analyzer-name plugin-paths)))
    (message "Analyzers found in %s : %s" plugin-dir plugin-names)
    plugin-names))

(defun lsp-sonarlint--check-enabled-analyzer(analyzer)
  "Return t if ANALYZER is enabled.
See `lsp-sonarlint-available-analyzers' and `lsp-sonarlint-enabled-analyzers'"
  (let* ((analyzer-name (lsp-sonarlint--extract-analyzer-name analyzer)))
    (pcase lsp-sonarlint-enabled-analyzers
      ('all t)
      (_ (seq-contains-p lsp-sonarlint-enabled-analyzers analyzer-name)))))

(defun lsp-sonarlint-server-start-fun()
  "Start lsp-sonarlint in stdio mode."
  ;; This will signal an error if `lsp-sonarlint-download-dir' is not a dir
  ;; or if the java executable is not found there.
  ;; Thus, it also serves as a check for the presence of SonarLint LSP server.
  (let* ((root-dir lsp-sonarlint-download-dir)
         (bundled-java-path (car (directory-files-recursively root-dir "java\\(.exe\\)?$")))
         (java-path (if lsp-sonarlint-use-system-jre "java" bundled-java-path))
         (jar-path (concat root-dir "/extension/server/sonarlint-ls.jar"))
         (analyzer-dir (concat root-dir "/extension/analyzers"))
         (analyzer-paths (directory-files analyzer-dir t ".jar"))
         (enabled-analyzers (seq-filter #'lsp-sonarlint--check-enabled-analyzer analyzer-paths)))
    (unless java-path
      (warn  "lsp-sonarlint : java is required and was not found on your system" :error))
    (-concat (list java-path "-jar" jar-path "-stdio" "-analyzers") enabled-analyzers)))

(defun lsp-sonarlint--code-action-open-rule (_workspace params)
  "Create a buffer with rendered rule from PARAMS text in it.
Extracts the title ahd htmlDescription, and renders the HTML in a
temporary buffer."
  (with-temp-buffer
    (let* ((rule-title (format "<h1>%s</h1>" (ht-get params "name")))
           (rule-summary (format "%s<hr/>" (ht-get params "htmlDescription")))
           (rule-body-array (ht-get params "htmlDescriptionTabs")))
      (insert rule-title)
      (insert rule-summary)
      (insert "\n")
      (seq-doseq (rule-body rule-body-array)
        (let ((rule-body-title (format "<h2>%s</h2>" (ht-get rule-body "title")))
             (rule-body-content (format "%s<hr/>" (ht-get (ht-get rule-body "ruleDescriptionTabNonContextual") "htmlContent"))))
          (insert rule-body-title)
          (insert "\n")
          (insert rule-body-content))))
    (shr-render-buffer (current-buffer))))

(defun lsp-sonarlint--analyze-folder(dirname)
  "Return t if DIRNAME should be analyzed.
i.e folders starting with '.' will be ignored"
  (not (string= (substring (file-name-base dirname) 0 1) ".")))

(defun lsp-sonarlint--invalid-file-name-encoding(file-path)
  "Return t if the file-path is not encodable as UTF-8.
This is important since emacs does not support non UTF-8 json serialization,
so a stray accentuated character encoded in a legacy filename in 8859-1
will raise an error for the whole project."
  (cl-some (lambda (ch)
             (not (encode-coding-char ch 'utf-8 'unicode)))
           file-path))

(defun lsp-sonarlint--list-files-in-folder (workspace _params)
  "Respond to a listFilesInFolder request.
List all files of interest in WORKSPACE's directory.
See `lsp-sonarlint-analyze-folder' to see which files are ignored."
  (let* ((root-dir (lsp--workspace-root workspace))
         (files (directory-files-recursively root-dir ".*" nil 'lsp-sonarlint--analyze-folder))
         (utf8-filenames (cl-remove-if #'lsp-sonarlint--invalid-file-name-encoding files)))
    (lsp-ht
     ("foundFiles"
      (apply 'vector
             (mapcar (lambda(file)
                       (lsp-ht
                        ("fileName" (file-name-nondirectory file))
                        ("filePath" file)))
                     utf8-filenames))))))

(defvar lsp-sonarlint--secondary-locations-overlays nil
  "List of overlays highlighting secondary locations.")

(defface lsp-sonarlint--step-marker '((((class color) (background light))
                                       (:weight bold
                                       :box (:line-width (2 . -2)
                                             :color "pink"
                                             :style released-button)
                                       :background "dark red"
                                       :foreground "white")))
  "Face used for the little markers on the side of each secondary step.")

(defface lsp-sonarlint-secondary-location-face
  '((((class color) (background light))
     (:background "wheat2"))
    (((class color) (background dark))
     (:background "gray7"))
    (t :inverse-video t))
  "Face used for the secondary locations of a SonarLint issue.")


(defface lsp-sonarlint-highlighted-secondary-face
  '((((class color) (background light))
     (:box (:line-width -1 :color red)
      :background "yellow"))
    (((class color) (background dark))
     (:box (:line-width -1 :color red)
      :background "blue3"))
    (t :inverse-video t))
  "Face used for the highlighted secondary location of a SonarLint issue.")

(defface lsp-sonarlint-primary-message-face
  '((t :inherit 'lsp-face-semhl-keyword))
  "Face used for the primary message in the list of secondary messages.")

(defface lsp-sonarlint-embedded-msg-face
  '((t :italic t :height 0.8))
  "Face used for the in-line secondary messages.")

(defun lsp-sonarlint--get-range-positions (range)
  "Convert the RANGE hash table from SonarLint to a plist with positions."
  (let ((start-line (1- (ht-get range "startLine")))
        (start-col (ht-get range "startLineOffset"))
        (end-line (1- (ht-get range "endLine")))
        (end-col (ht-get range "endLineOffset")))
    `(:begin ,(lsp--line-character-to-point start-line start-col)
      :end ,(lsp--line-character-to-point end-line end-col))))

(defun lsp-sonarlint--make-overlay-between (begin-end-positions)
  "Create an overlay between BEGIN-END-POSITIONS.

BEGIN-END-POSITIONS is a plist with :begin and :end positions."
  (let* ((start-pos (plist-get begin-end-positions :begin))
         (end-pos (plist-get begin-end-positions :end))
         (overlay (make-overlay start-pos end-pos (current-buffer))))
    (push overlay lsp-sonarlint--secondary-locations-overlays)
    overlay))

(defun lsp-sonarlint--add-number-marker (overlay num)
  "Add a prefix marker NUM to OVERLAY."
  (overlay-put overlay 'before-string
               (propertize (number-to-string num)
                           'face 'lsp-sonarlint--step-marker)))

(defun lsp-sonarlint--make-full-line-overlay (line)
  "Create an overlay covering entire LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (- line 1))
    (lsp-sonarlint--make-overlay-between `(:begin ,(line-beginning-position)
                                           :end ,(line-end-position)))))

(defun lsp-sonarlint--get-column (pos)
  "Get the column of the point position POS."
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun lsp-sonarlint--procure-overlays-for-secondary-locations (flows)
  "Create overlays for secondary locations in FLOWS.

Returns a list of plists with the overlay, step number, and message."
  (let ((step-num 0))
    (apply
     #'append
     (seq-map
      (lambda (flow)
        ;; By SonarLint convention in which the execution flow leading to a
        ;; data-flow issue is listed in reverse order. `seq-reverse' lets us
        ;; display the execution flow in natural order.
        (let ((locations (seq-reverse (ht-get flow "locations"))))
          (seq-map
           (lambda (location)
             (setq step-num (1+ step-num))
             (let* ((range-ht (ht-get location "textRange"))
                    (range (lsp-sonarlint--get-range-positions range-ht))
                    (overlay (lsp-sonarlint--make-overlay-between range))
                    (message (ht-get location "message")))
               (overlay-put overlay 'face 'lsp-sonarlint-secondary-location-face)
               (lsp-sonarlint--add-number-marker overlay step-num)
               `(:overlay ,overlay :step-num ,step-num :message ,message)))
           locations)))
      flows))))

(defun lsp-sonarlint--procure-overlay-for-primary-location (action-args)
  "Parse ACTION-ARGS for the primary location and  message and place an overlay."
  (let* ((message (ht-get action-args "message"))
         (range-ht (ht-get action-args "textRange"))
         (range (lsp-sonarlint--get-range-positions range-ht))
         (overlay (lsp-sonarlint--make-overlay-between range)))
    `(:overlay ,overlay :message ,message)))

(defconst lsp-sonarlint--secondary-messages-buffer-name "*SonarLint secondary locations*"
  "Name of the buffer where messages for secondary locations are displayed.")

(defun lsp-sonarlint--remove-secondary-loc-highlights ()
  "Remove all overlays highlighting secondary locations."
  (mapc #'delete-overlay lsp-sonarlint--secondary-locations-overlays)
  (setq lsp-sonarlint--secondary-locations-overlays nil))

(defun lsp-sonarlint--on-quit-window ()
  "Remove locations' overlays when the secondary-messages window is closed."
  (when (string-equal (buffer-name) lsp-sonarlint--secondary-messages-buffer-name)
    (lsp-sonarlint--remove-advices)
    (lsp-sonarlint--remove-secondary-loc-highlights)))

(defun lsp-sonarlint--remove-advices ()
  "Remove advices added to `line-move' and `mouse-set-point'."
    (advice-remove 'line-move #'lsp-sonarlint--on-line-move)
    (advice-remove 'mouse-set-point #'lsp-sonarlint--on-line-move)
    (advice-remove 'kill-buffer #'lsp-sonarlint--on-kill-buffer))

(defvar lsp-sonarlint--previously-focused-overlays nil
  "The list of overlays that was previously focused.")

(defun lsp-sonarlint--unfocus-overlays ()
  "Unfocus the previously focused overlays."
  (dolist (ov lsp-sonarlint--previously-focused-overlays)
    (overlay-put ov 'face 'lsp-sonarlint-secondary-location-face))
  (setq lsp-sonarlint--previously-focused-overlays nil))

(defun lsp-sonarlint--highlight-target (overlay)
  "Highlight OVERLAY."
  (lsp-sonarlint--unfocus-overlays)
  (overlay-put overlay 'face 'lsp-sonarlint-highlighted-secondary-face)
  (push overlay lsp-sonarlint--previously-focused-overlays))

(defun lsp-sonarlint--focus-on-target (overlay)
  "Put point to OVERLAY and make it visible in another window."
  (when-let ((target-buffer (overlay-buffer overlay))
             (prev-buffer (current-buffer)))
    (switch-to-buffer-other-window target-buffer)
    (goto-char (overlay-start overlay))
    (hl-line-highlight) ; make sure the line highlighting is updated
    (switch-to-buffer-other-window prev-buffer)))

(defvar lsp-sonarlint--original-buffer nil
  "The buffer with code and SonarLint issues.

Useful when exploring secondary locations, which uses an auxiliary buffer.")

(defun lsp-sonarlint--on-line-move (&rest _args)
  "Highlight the current line in the secondary locations buffer."
  (when (string-equal (buffer-name) lsp-sonarlint--secondary-messages-buffer-name)
    (let ((focus-overlay nil))
      (mapc (lambda (ovl) (when (overlay-get ovl 'focus-location)
                       (setq focus-overlay (overlay-get ovl 'focus-location))))
            (overlays-at (point)))
      (when focus-overlay
        (lsp-sonarlint--focus-on-target focus-overlay)
        (lsp-sonarlint--highlight-target focus-overlay))))
  (when (eq (current-buffer) lsp-sonarlint--original-buffer)
    (when-let* ((focus-overlay (seq-find (lambda (ovl) (overlay-get ovl 'lsp-sonarlint--message-overlay))
                                         (overlays-at (- (point) 1) t)))
                (message-overlay (overlay-get focus-overlay 'lsp-sonarlint--message-overlay)))
      (lsp-sonarlint--focus-on-target message-overlay)
      (lsp-sonarlint--highlight-target message-overlay)
      (hl-line-highlight))))


(defun lsp-sonarlint--on-kill-buffer (&rest _args)
  "Remove sec-locations' highlights and advices when the buffer is killed."
  (lsp-sonarlint--on-quit-window))

(defun lsp-sonarlint--add-message-entry (loc-message)
  "Add a new line with message from LOC-MESSAGE.

Should be called on an empty line.
Returns an overlay placed across the line that has the `focus-location'
pointing to the `:overlay' from LOC-MESSAGE."
  (insert (plist-get loc-message :message))
  (let ((overlay (lsp-sonarlint--make-overlay-between
                  `(:begin ,(line-beginning-position)
                    :end ,(line-end-position))))
        (focus-overlay (plist-get loc-message :overlay)))
    (overlay-put overlay 'focus-location focus-overlay)
    (overlay-put focus-overlay 'lsp-sonarlint--message-overlay overlay)
    overlay))

(defun lsp-sonarlint--extract-located-messages (locations)
  "Group messages from LOCATIONS by their coordinates."
  (let ((line-to-msg (make-hash-table :test #'equal)))
    (mapc (lambda (location)
            (let* ((precise-overlay (plist-get location :overlay))
                   (message-offset (lsp-sonarlint--get-column (overlay-start precise-overlay)))
                   (line (line-number-at-pos (overlay-start precise-overlay))))
              (push `(:offset ,message-offset ,@location)
                    (gethash line line-to-msg))))
          locations)
    line-to-msg))

(defun lsp-sonarlint--deduplicate (sorted-list test)
  "Remove duplicate elements (according to TEST) from sorted SORTED-LIST."
  (let ((result '())
        (last-element nil))
    (dolist (element sorted-list (nreverse result))
      (unless (funcall test element last-element)
        (push element result)
        (setq last-element element)))))

(defun lsp-sonarlint--combine (messages-with-offsets)
  "Combine MESSAGES-WITH-OFFSETS that don't overlap into single line.

MESSAGES-WITH-OFFSETS must be sorted by offset."
  (let ((result '())
        (reversed (reverse messages-with-offsets)))
    (dolist (msg-off messages-with-offsets (nreverse result))
      (when-let* ((right-most (car reversed))
                  (right-offset (plist-get right-most :offset))
                  (my-offset (plist-get msg-off :offset))
                  (my-message (plist-get msg-off :message))
                  (gap (- right-offset (+ my-offset (length my-message)))))
        (unless (< right-offset my-offset) ; this element is already combined
          (if (<= gap 3) ;; too close or overlap
              (push msg-off result)
            (setf (plist-get msg-off :message)
                  (concat my-message
                          (make-string gap ?\s)
                          (plist-get right-most :message)))
            (pop reversed)
            (push msg-off result)))))))

(defun lsp-sonarlint--count-digits (num)
  "Count digits in decimal representation of the NUM integer."
  (length (number-to-string num)))

(defun lsp-sonarlint--adjust-offsets (messages-with-offsets)
  "Shift offsets in MESSAGES-WITH-OFFSETS to account for number labels.

MESSAGES-WITH-OFFSETS must be sorted."
  (let ((accumulated-adjustment 0))
    (mapcar (lambda (msg-with-offset)
              (let ((increment
                     (if-let ((step-num (plist-get
                                         msg-with-offset
                                         :step-num)))
                         (lsp-sonarlint--count-digits step-num)
                       0)))
                (setq accumulated-adjustment (+ accumulated-adjustment increment))
                (setf (plist-get msg-with-offset :offset)
                      (+ accumulated-adjustment
                         (plist-get msg-with-offset :offset)))
                msg-with-offset))
            messages-with-offsets)))

(defcustom lsp-sonarlint--scale-inline-msg-offset t
  "Whether to scale the offset for inline messages (code-lens style).

Usually these messages (including their whitespace offset) are
printed with smaller font, so they need adjustment to account for
smaller size of the space character.

- Set to nil if it causes problems.
- Set to a floating-point number if you want to adjust this factor.
- If t it will deduce the scaling factor
  from the `lsp-sonarlint-embedded-msg-face' height."
  :type '(choice
          (const :tag "Disable" nil)
          (integer :tag "Factor")
          (const :tag "Auto" t)))

(defun lsp-sonarlint--scale-offset (offset)
  "Adjust OFFSET preserving column position with smaller font."
  (if lsp-sonarlint--scale-inline-msg-offset
      (if (numberp lsp-sonarlint--scale-inline-msg-offset)
          (floor (* lsp-sonarlint--scale-inline-msg-offset offset))
        (let ((msg-height (face-attribute 'lsp-sonarlint-embedded-msg-face :height nil 'default))
              (default-height (face-attribute 'default :height)))
          (/ (* offset
                default-height)
             msg-height)))
    offset))

(defun lsp-sonarlint--scale-msg-lens-offset (msg-with-offset)
  "Adjust offset in MSG-WITH-OFFSET preserving column with smaller font."
  (setf (plist-get msg-with-offset :offset)
        (lsp-sonarlint--scale-offset (plist-get msg-with-offset :offset)))
  msg-with-offset)

(defun lsp-sonarlint--process-offsets (messages-with-offsets)
  "Sort, deduplicate, adjust, and combine MESSAGES-WITH-OFFSETS.

Sort them in increasing order, remove duplicate messages with identical offsets,
adjust offsets to account for the number labels prepended to each location,
and combine non-overlapping messages toreduce the number of lines."
  (let* ((sorted (sort messages-with-offsets (lambda (msg-off1 msg-off2)
                                               (< (plist-get msg-off1 :offset)
                                                  (plist-get msg-off2 :offset)))))
         (deduplicated (lsp-sonarlint--deduplicate
                        sorted
                        (lambda (msg-off1 msg-off2) (and (equal (plist-get msg-off1 :offset)
                                                           (plist-get msg-off2 :offset))
                                                    (equal (plist-get msg-off1 :message)
                                                           (plist-get msg-off2 :message))))))
         (adjusted (lsp-sonarlint--adjust-offsets deduplicated))
         ;; Should scale after adjusting, because adjustment is done
         ;; in terms of the default font
         (scaled (mapcar #'lsp-sonarlint--scale-msg-lens-offset adjusted)))
    ;; Should combine after scaling to also scale the potential gaps between
    ;; combined messages properly
    (lsp-sonarlint--combine scaled)))

(defun lsp-sonarlint--concat-msg-lines (msg-offsets)
  "Combine the list of MSG-OFFSETS into a single string."
  (string-join
   (mapcar (lambda (msg-offset)
             (concat (make-string (plist-get msg-offset :offset) ?\s)
                     (plist-get msg-offset :message)))
           msg-offsets)
   "\n"))

(defun lsp-sonarlint--add-inline-messages (locations)
  "Add lens-style in-line messages for LOCATIONS."
  (maphash (lambda (line messages)
             (let* ((adjusted-messages
                     (lsp-sonarlint--process-offsets messages))
                    (overlay (lsp-sonarlint--make-full-line-overlay line))
                    (prefix-count (/ (1+ (length adjusted-messages)) 2))
                    (prefix-msgs (seq-take adjusted-messages prefix-count))
                    (postfix-msgs (seq-drop adjusted-messages prefix-count)))
               (when prefix-msgs
                 (overlay-put overlay 'before-string
                              (propertize (concat (lsp-sonarlint--concat-msg-lines prefix-msgs)
                                                  "\n")
                                          'face 'lsp-sonarlint-embedded-msg-face)))
               (when postfix-msgs
                 (overlay-put overlay 'after-string
                              (propertize (concat "\n"
                                                  (lsp-sonarlint--concat-msg-lines postfix-msgs))
                                          'face 'lsp-sonarlint-embedded-msg-face)))))
           (lsp-sonarlint--extract-located-messages locations)))

(defun lsp-sonarlint--show-all-locations (command)
  "Show all secondary locations listed in COMMAND for the focused issue."
  (lsp-sonarlint--remove-secondary-loc-highlights)
  (let* ((arguments (seq-first (ht-get command "arguments")))
         (flows (ht-get arguments "flows")))
    (let ((locations (lsp-sonarlint--procure-overlays-for-secondary-locations flows))
          (primary (lsp-sonarlint--procure-overlay-for-primary-location arguments)))
      (setq lsp-sonarlint--original-buffer (current-buffer))
      (switch-to-buffer-other-window lsp-sonarlint--secondary-messages-buffer-name)
      (fundamental-mode)
      (setq buffer-read-only nil)
      (erase-buffer)

      (let ((prim-overlay (lsp-sonarlint--add-message-entry primary)))
        (overlay-put prim-overlay 'face 'lsp-sonarlint-primary-message-face))

      (advice-add 'line-move :after #'lsp-sonarlint--on-line-move)
      (advice-add 'mouse-set-point :after #'lsp-sonarlint--on-line-move)
      (advice-add 'kill-buffer :after #'lsp-sonarlint--on-kill-buffer)

      (dolist (location locations)
        (insert "\n")
        (let ((overlay (lsp-sonarlint--add-message-entry location)))
          (lsp-sonarlint--add-number-marker overlay (plist-get location :step-num))))
      (with-current-buffer lsp-sonarlint--original-buffer
        (lsp-sonarlint--add-inline-messages (cons primary locations)))
      (goto-char (point-min))
      (tabulated-list-mode)
      (setq-local cursor-type nil))))

(defvar lsp-sonarlint--action-handlers
  (lsp-ht
   ("SonarLint.ShowAllLocations" #'lsp-sonarlint--show-all-locations)))

(lsp-register-custom-settings
 '(("sonarlint.disableTelemetry" lsp-sonarlint-disable-telemetry)
   ("sonarlint.testFilePattern" lsp-sonarlint-test-file-pattern)
   ("sonarlint.pathToCompileCommands" lsp-sonarlint-cfamily-compile-commands-path)
   ("sonarlint.output.showAnalyzerLogs" lsp-sonarlint-show-analyzer-logs)
   ("sonarlint.output.verboseLogs" lsp-sonarlint-verbose-logs)
   ("sonarlint.ls.vmargs" lsp-sonarlint-vmargs)))

(defvar lsp-sonarlint--request-handlers
  (lsp-ht
   ;; Check whether the file should be analyzed or not
   ;; This is typically to check if an open file is in an active workspace or not
   ;; Let's assume here that the file should be analyzed
   ("sonarlint/shouldAnalyseFile" (lambda (&rest _)
                                    (lsp-ht
                                     ("shouldBeAnalysed" t))))
   ;; Sonarlint sends those before to ask if you want to display a notification
   ;; On what your environement is missing. e.g If you have an old version of nodejs.
   ("sonarlint/canShowMissingRequirementsNotification" (lambda (&rest _) t))
   ;; For some reason, sonarlint requests the list of files in the folder
   ;; VSCode's extension seems to send everything, even the .git folder.
   ;; Here we give anything stored inside of the root directory, except
   ;; hidden folders (in the unix sense, those which start with '.')
   ("sonarlint/listFilesInFolder" #'lsp-sonarlint--list-files-in-folder)
   ;; Sonarlint VSCode extension has a setting to specify a glob pattern
   ;; of files to be excluded from analysis.
   ;; We do not support this option at the moment, do not filter anything.
   ("sonarlint/filterOutExcludedFiles" (lambda (_ params) params))
   ;; Check if the file is ignored by Source Control Manager (a.k.e. VCS, version control system).
   ;; I think this is related to .gitignore and similar files.
   ;; Probably safe to assume as a first step that you don't care, and want your diagnostics.
   ;; TODO: implement a proper check here.
   ("sonarlint/isIgnoredByScm" #'ignore)
   ;; Probably only relevant to the java analyzer.
   ;; Some additional java configuration for the project.
   ;; TODO: implement
   ("sonarlint/getJavaConfig" #'ignore))

  "SonarLint-specific request handlers.
See REQUEST-HANDLERS in lsp--client in lsp-mode."
  )

(defvar lsp-sonarlint--notification-handlers
  (lsp-ht
   ;; Security Hotspots are a special kind of issue that have particular
   ;; interface on SonarCloud, SonarQube, and in SonarLint. See
   ;; https://docs.sonarcloud.io/digging-deeper/security-hotspots/ I presume
   ;; the PARAMS contain a list of issues of this category, similar to the
   ;; normal issues.
   ;; TODO: display them, perhaps optionally, as they could be noisy sometimes,
   ;; especially without the possibility to "review" them once and forever.
   ("sonarlint/publishSecurityHotspots" #'ignore)
   ;; Sonarlint sends this to suggest the connected mode, and sends along
   ;; your previous sonar projects. Connected mode is not currently implemented here.
   ("sonarlint/suggestConnection" #'ignore)
   ;; Not sure what this is for. Testing of SonarLint itself?
   ("sonarlint/readyForTests" #'ignore)
   ;; Sent by cfamily for analysis of C/C++ files. Sonar requires your
   ;; build commands specified in a compile_commands.json
   ("sonarlint/needCompilationDatabase" (lambda(&rest _)
                                          (warn "Sonar could not find your compile_commands.json, please check `lsp-sonarlint-cfamily-compile-commands-path'")))
   ;; This is probably just to raise awareness of the new kind of issues:
   ;; secrets. That'd be too booring to implement. Hopefully, the user is
   ;; paying attention and will notice anyway.
   ("sonarlint/showNotificationForFirstSecretsIssue" #'ignore)
   ("sonarlint/showRuleDescription" #'lsp-sonarlint--code-action-open-rule))

  "SonarLint-specific notification handlers.
See NOTIFICATION-HANDLERS in lsp--client in lsp-mode.")

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-sonarlint-server-start-fun)
  :major-modes lsp-sonarlint-modes-enabled
  :priority -1
  :request-handlers lsp-sonarlint--request-handlers
  :notification-handlers lsp-sonarlint--notification-handlers
  :multi-root t
  :add-on? t
  :server-id 'sonarlint
  :action-handlers lsp-sonarlint--action-handlers
  :initialization-options (lambda ()
                            (list
                             :productKey "emacs"
                             :productName "Emacs"))
  :after-open-fn (lambda ()
                   (add-hook 'quit-window-hook #'lsp-sonarlint--on-quit-window))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "sonarlint"))))))

(provide 'lsp-sonarlint)
;;; lsp-sonarlint.el ends here
