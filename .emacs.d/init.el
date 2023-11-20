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
   '("bc7d4cfb6d4bd7074a39f97f0b8a057c5b651c403950bbbc4ad35a609ad6268a" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "c5ded9320a346146bbc2ead692f0c63be512747963257f18cc8518c5254b7bf5" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "1f1b545575c81b967879a5dddc878783e6ebcca764e4916a270f9474215289e5" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "a6e620c9decbea9cac46ea47541b31b3e20804a4646ca6da4cce105ee03e8d0e" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "6c98bc9f39e8f8fd6da5b9c74a624cbb3782b4be8abae8fd84cbc43053d7c175" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "4b6b6b0a44a40f3586f0f641c25340718c7c626cbf163a78b5a399fbe0226659" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "b186688fbec5e00ee8683b9f2588523abdf2db40562839b2c5458fcfb322c8a4" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "6f4421bf31387397f6710b6f6381c448d1a71944d9e9da4e0057b3fe5d6f2fad" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" default))
 '(exwm-floating-border-color "#3d3f4d")
 '(fci-rule-color "#595d68")
 '(highlight-indent-guides-method 'character)
 '(highlight-tail-colors ((("#363b3d") . 0) (("#333a46") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#131313" "#ffd76d"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#131313" "#bad761"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#131313" "#535763"))
 '(lsp-java-java-path "/Users/hs/.sdkman/candidates/java/current/bin/java")
 '(objed-cursor-color "#ff657a")
 '(package-selected-packages
   '(ein jupyter chatgpt-shell ns-auto-titlebar good-scroll dap-mode treemacs lsp-ui go-mode dtrt-indent counsel-etags counsel-gtags highlight-indent-guides kubernetes-tramp kubernetes-helm kubernetes fontawesome elpy lsp-pyright dashboard python-mode pyenv-mode company-terraform terraform-doc terraform-mode yapfify sphinx-doc yasnippet-snippets yasnippet-classic-snippets eslint-fix eslintd-fix bazel jenkinsfile-mode scala-mode yaml-mode js-format auto-indent-mode dockerfile-mode kotlin-mode cmake-mode cpputils-cmake tramp-theme ivy-posframe which-key wgrep-ag web-mode use-package tide smex smartparens rainbow-delimiters php-mode org-onenote org-download org-bullets multiple-cursors magit lsp-ivy js2-mode js-doc ivy-rich goto-chg git-timemachine expand-region exec-path-from-shell editorconfig dumb-jump drag-stuff doom-themes doom-modeline dired-subtree dired-git-info counsel-projectile company-box centaur-tabs bm anzu ag ace-window))
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
 '(vc-annotate-very-old-color nil)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-tests-directory "~/tests/")
 '(zoom-window-mode-line-color "DarkGreen"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
