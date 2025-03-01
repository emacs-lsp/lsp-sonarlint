(package "lsp-sonarlint"
         "0.0.1"
         "Emacs SonarLint lsp client")

(website-url "https://github.com/emacs-lsp/lsp-sonarlint")
(keywords "languages" "tools" "php" "javascript" "typescript" "go" "xml" "html" "java" "python")

(package-file "lsp-sonarlint.el")

(files "languages" "languages/*/*.el" "server" "server/*")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "28.1")
(depends-on "dash" "2.12.0")
(depends-on "lsp-mode" "6.3")
(depends-on "ht" "2.3")

(development
  (depends-on "ert"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
