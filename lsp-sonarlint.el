;;; lsp-sonarlint.el --- Emacs SonarLint lsp client              -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Fermin Munoz

;; Author: Fermin MF <fmfs@posteo.net>
;; Created: 13 Jun 2020
;; Version: 0.0.1
;; Keywords: languages, tools, php, javascript, typescript, go, xml, html, java, python
;; URL: https://github.com/emacs-lsp/lsp-sonarlint
;; Package-Requires: ((emacs "27.1") (dash "2.12.0") (lsp-mode "6.3") (ht "2.3"))
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

(defcustom-lsp lsp-sonarlint-disable-telemetry t
  "Disable sending anonymous usage statistics to SonarSource.
To see a sample of the data that are collected
https://github.com/SonarSource/sonarlint-vscode/blob/master/telemetry-sample.md."
  :group 'lsp-sonarlint
  :type 'boolean
  :lsp-path "sonarlint.disableTelemetry")

(defcustom-lsp lsp-sonarlint-test-file-pattern "{**/test/**,**/*test*,**/*Test*}"
  "Files whose name match java global are considered as test files by analyzers.
Most rules are not evaluated on test files.
Example: `{**/test/**,**/*test*,**/*Test*}`"
  :group 'lsp-sonarlint
  :type 'string
  :lsp-path "sonarlint.testFilePattern")

(defcustom-lsp lsp-sonarlint-show-analyzer-logs nil
  "Show analyzer's logs in the SonarLint output."
  :group 'lsp-sonarlint
  :type 'boolean
  :lsp-path "sonarlint.output.showAnalyzerLogs")

(defcustom-lsp lsp-sonarlint-verbose-logs nil
  "Enable verbose logging of the SonarLint language server."
  :group 'lsp-sonarlint
  :type 'boolean
  :lsp-path "sonarlint.output.verboseLogs")

(defcustom-lsp lsp-sonarlint-vmargs ""
  "Extra JVM arguments used to launch the SonarLint LSP.
e.g. `-Xmx1024m`."
  :group 'lsp-sonarlint
  :type 'string
  :lsp-path "sonarlint.ls.vmargs")

 (defun lsp-sonarlint--find-file-in-parent-folders (fname)
   "Find the closest FNAME in a buffer folder or one of its parents.
 Traverse the parent folders from narrow to wide (/a/b/c, /a/b, /a, /).
 If none of them contains FNAME, return nil."
   (let ((dir (file-name-directory (expand-file-name (buffer-file-name))))
         (found-file nil))
     (while (not (or found-file (string-empty-p dir) (string-equal dir "/")))
       (let ((potential-file (concat dir fname)))
         (if (file-exists-p potential-file)
             (setq found-file potential-file)
           (setq dir (file-name-directory (directory-file-name dir))))))
     found-file))

(defcustom-lsp lsp-sonarlint-path-to-compile-commands ""
  "Path to the compilation DB - the compile_commands.json file."
  :type 'path
  :group 'lsp-sonarlint
  :lsp-path "sonarlint.pathToCompileCommands")

 (defun lsp-sonarlint--get-compile-commands ()
   "Find compile_commands.json in the parent dir or ask user."
   (or (lsp-sonarlint--find-file-in-parent-folders "compile_commands.json")
       (read-file-name "Provide path to compile_commands.json for this project: ")))

 (defun lsp-sonarlint--set-compile-commands (_workspace _params)
   "Find compile_commands.json and set it for the workspace.
 As a side effect it will also send the found path to the SonarLint server."
   (let ((fname (lsp-sonarlint--get-compile-commands)))
     (unless (string-empty-p fname)
       (message "Using compilation database from %s." fname)
       (custom-set-variables `(lsp-sonarlint-path-to-compile-commands ,(expand-file-name fname))))))

(defun lsp-sonarlint-set-compile-commands (fname)
  "Set FNAME as the path to the compile_commands.json file for current session."
  (interactive "fCompilation DB path: ")
  (custom-set-variables `(lsp-sonarlint-path-to-compile-commands ,(expand-file-name fname))))

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

(defvar lsp-sonarlint--action-handlers '())

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
   ("sonarlint/needCompilationDatabase" #'lsp-sonarlint--set-compile-commands)
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
  :multi-root nil
  :add-on? t
  :server-id 'sonarlint
  :action-handlers (ht<-alist lsp-sonarlint--action-handlers)
  :initialization-options (lambda ()
                            (list
                             :productKey "emacs"
                             :productName "Emacs"))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "sonarlint"))))
  :synchronize-sections '("sonarlint")))

(provide 'lsp-sonarlint)
;;; lsp-sonarlint.el ends here
