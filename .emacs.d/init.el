
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "loader.org"
                   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282a3a" "#ff657a" "#bad761" "#ffd76d" "#9cd1bb" "#9cd1bb" "#9cd1bb" "#eaf2f1"])
 '(custom-safe-themes
   (quote
    ("b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "6f4421bf31387397f6710b6f6381c448d1a71944d9e9da4e0057b3fe5d6f2fad" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" default)))
 '(exwm-floating-border-color "#3d3f4d")
 '(fci-rule-color "#595d68")
 '(highlight-tail-colors ((("#363b3d") . 0) (("#333a46") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#131313" "#ffd76d"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#131313" "#bad761"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#131313" "#535763"))
 '(objed-cursor-color "#ff657a")
 '(package-selected-packages
   (quote
    (all-the-icons-ivy lsp-pyright dashboard python-mode pyenv-mode company-terraform terraform-doc terraform-mode lsp-jedi yapfify sphinx-doc yasnippet-snippets yasnippet-classic-snippets eslint-fix eslintd-fix bazel jenkinsfile-mode scala-mode yaml-mode js-format auto-indent-mode dockerfile-mode kotlin-mode cmake-mode cpputils-cmake tramp-theme ivy-posframe which-key wgrep-ag web-mode use-package tide smex smartparens rainbow-delimiters php-mode org-onenote org-download org-bullets multiple-cursors magit lsp-ui lsp-ivy js2-mode js-doc ivy-rich goto-chg git-timemachine expand-region exec-path-from-shell editorconfig dumb-jump drag-stuff doom-themes doom-modeline dired-subtree dired-git-info counsel-projectile company-box centaur-tabs bm anzu ag ace-window)))
 '(pdf-view-midnight-colors (cons "#eaf2f1" "#282a3a"))
 '(rustic-ansi-faces
   ["#282a3a" "#ff657a" "#bad761" "#ffd76d" "#9cd1bb" "#9cd1bb" "#9cd1bb" "#eaf2f1"])
 '(vc-annotate-background "#282a3a")
 '(vc-annotate-color-map
   (list
    (cons 20 "#bad761")
    (cons 40 "#d1d765")
    (cons 60 "#e8d769")
    (cons 80 "#ffd76d")
    (cons 100 "#ffc188")
    (cons 120 "#ffaba3")
    (cons 140 "#ff95be")
    (cons 160 "#dea9bd")
    (cons 180 "#bdbdbb")
    (cons 200 "#9cd1bb")
    (cons 220 "#bdada5")
    (cons 240 "#de898f")
    (cons 260 "#ff657a")
    (cons 280 "#d46174")
    (cons 300 "#a95e6e")
    (cons 320 "#7e5a68")
    (cons 340 "#595d68")
    (cons 360 "#595d68")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
