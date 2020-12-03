(if (eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier 'none)
  )

(setq auto-window-vscroll nil)
;; (global-display-line-numbers-mode t)

;; Don't display welcom page
(setq inhibit-startup-message t)

(menu-bar-mode 0)

(tool-bar-mode 0)

(setq backup-directory-alist '(("." . "~/.emacs_saves")))

;; auto complete when find file
(electric-pair-mode)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq column-number-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
;; Show match parenthesis
(show-paren-mode 1)

;; Don't display welcom page
(setq inhibit-startup-message t)

;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)

;; Start at full-screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 130
                    :weight 'normal
                    :width 'normal)


(require 'misc)
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-e") 'forward-word)

(global-set-key (kbd "C-; C-z") 'toggle-maximize-buffer)
(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_) 
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))


(require 'package)
(setq package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa")
			 ("elpag" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize))
  )


;; smart M-x
(use-package smex)

(use-package ivy
  :diminish
  :bind
  (("M-o" . swiper-all-thing-at-point)
   ("M-x" . counsel-M-x)
   ("C-; L" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   ("C-c M-o" . counsel-multi)
   :map ivy-minibuffer-map
   )
  :config
  (ivy-mode 1)
  )

(use-package doom-modeline
  :init (doom-modeline-mode 1))


(use-package rainbow-delimiters
  :hook (my-program-mode-hook . rainbom-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


(use-package ivy-rich
  :diminish
  :config
  (ivy-rich-mode t))


(use-package doom-themes
  :config
  (load-theme 'doom-solarized-light t)
  )


(global-set-key (kbd "C-; b p")  'previous-buffer)
(global-set-key (kbd "C-; b n")  'next-buffer)
(global-set-key "\M-N" "\C-u1\C-v")
(global-set-key "\M-P" "\C-u1\M-v")

;; Workaround for copy
(setq save-interprogram-paste-before-kill t)

;; show killring
(global-set-key (kbd "C-; y") 'counsel-yank-pop)

;; copy files
(setq dired-dwim-target t)

;; use spaces over tabs
(setq-default indent-tabs-mode nil)

(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)

;; kill buffer after delete
(defun delete-file-visited-by-buffer (buffername)
  "Delete the file visited by the buffer named BUFFERNAME."
  (interactive "b")
  (let* ((buffer (get-buffer buffername))
         (filename (buffer-file-name buffer)))
    (when filename
      (delete-file filename)
      (kill-buffer-ask buffer))))



(add-hook 'org-mode-hook (lambda () (org-indent-mode t)))
(add-hook 'org-mode-hook #'toggle-truncate-lines)

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; Projectile
(use-package projectile
  :init
  (setq projectile-require-project-root nil)
  (projectile-mode t)
  )

(use-package counsel-projectile
  :bind
  (
   ("C-S-o" . counsel-projectile-switch-project)
   ("C-; C-f" . counsel-projectile-find-file)
   ("C-; l" . counsel-projectile-switch-to-buffer)
   )
  )

(use-package ag)

;; jump
(use-package dumb-jump
  :init
  (bind-key "C-; ]" 'dumb-jump-go)
  (bind-key "C-; t" 'dumb-jump-back))

;; Show search index
(use-package anzu
  :init
  (global-anzu-mode +1))

;; check syntax
(use-package flycheck
  :init (global-flycheck-mode 1))

(use-package bm
  :config
  (global-set-key (kbd "C-; m m") 'bm-toggle)
  (global-set-key (kbd "C-; m n")   'bm-next)
  (global-set-key (kbd "C-; m p") 'bm-previous))


;; Magit
(use-package magit
  :init
  (bind-key "C-x g" 'magit-status)
  (bind-key "C-; d" 'magit-diff-buffer-file))

(use-package goto-chg
  :bind
  (("C-o" . goto-last-change)
  ("C-i" . goto-last-change-reverse)))

(use-package avy
  :init
  (bind-key "C-; j" 'avy-goto-char))


(use-package ace-window
  :config
  
  (global-set-key (kbd "C-x o") 'ace-window)
  :init
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )


(use-package multiple-cursors
  :config)
;; (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))


;; Expand selection
(use-package expand-region
  :config
  (global-set-key (kbd "C-'") 'er/expand-region))


(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

;; attach image to orgmode

(use-package org-download)
;;              :ensure t)


(use-package dired-git-info
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))


(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . efs/lsp-mode-setup)
  ((typescript-mode js2-mode web-mode elpy-mode php-mode) . lsp)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-completion-provider :capf)
  :bind
  (:map lsp-mode-map
         ("TAB" . completion-at-point))
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (my-program-mode-hook . company-mode)
  :bind
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  (:map company-active-map ("<tab>" . company-complete-selection))
  (:map company-active-map ("C-n" . company-select-next))
  (:map company-active-map ("C-p" . company-select-previous))
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.0)
  )

(use-package company-box)

(use-package company
  :hook (company-mode . company-box-mode))

;; (use-package company-web)
;; (add-hook 'after-init-hook 'global-company-mode)


(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook
  (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(add-hook 'typescript-mode-hook #'setup-tide-mode)



(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))


(use-package elpy
  :init
  ;;  (exec-path-from-shell-initialize)
  (elpy-enable))

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys)
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

(use-package php-mode)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
  (add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode)) ;; auto-enable for .js/.jsx files
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  )

(use-package smartparens)



(defun my-program-mode-hook ()
  (hs-minor-mode)
  (local-set-key (kbd "C-+") 'hs-show-all) ;; ctrl+shift+=
  (local-set-key (kbd "C-_") 'hs-hide-all)   ;; ctrl+shift+-
  (local-set-key (kbd "C-=") 'hs-show-block)
  (local-set-key (kbd "C--") 'hs-hide-block)
  (which-function-mode t)
  (smartparens-mode t)
  )
(add-hook 'typescript-mode 'my-program-mode-hook)
(add-hook 'js-mode-hook 'my-program-mode-hook)
(add-hook 'web-mode-hook 'my-program-mode-hook)
;;(add-hook 'vue-mode-hook 'my-program-mode-hook)
(setenv "WORKON_HOME" "~/d1/anaconda3/envs")
(pyvenv-mode 1)

(add-hook 'php-mode-hook 'my-program-mode-hook)
(add-hook 'python-mode-hook 'my-program-mode-hook)
(add-hook 'python-mode-hook 'anaconda-mode)
(setq jedi:environment-root "/media/data/anaconda3/envs/mlenv")
(setq elpy-rpc-virtualenv-path "/media/data/anaconda3/envs/mlenv")

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (setq typescript-indent-level 2)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(lsp-mode which-key web-mode use-package tide ssass-mode smex smartparens rainbow-delimiters php-mode org-download org-bullets nameframe-perspective multiple-cursors mmm-mode magit leuven-theme js2-mode js-doc iy-go-to-char ivy-rich helm-swoop helm-projectile helm-perspeen helm-ag groovy-mode goto-chg go-mode git-timemachine fira-code-mode expand-region elpy edit-indirect dumb-jump dtrt-indent drag-stuff doom-themes doom-modeline docker-compose-mode dired-subtree dired-git-info counsel-projectile company-ycmd company-web company-jedi company-anaconda centaur-tabs bm anzu ag ace-window)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#222222" "#aa4450" "#87875f" "#cc8800" "#87AFD7" "#8787AF" "#87ceeb" "#c2c2b0"])
 '(custom-safe-themes
   '("e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "6084dce7da6b7447dcb9f93a981284dc823bab54f801ebf8a8e362a5332d2753" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "74ba9ed7161a26bfe04580279b8cad163c00b802f54c574bfa5d924b99daa4b9" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "188fed85e53a774ae62e09ec95d58bb8f54932b3fd77223101d036e3564f9206" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" default))
 '(fci-rule-color "#62686E")
 '(jdee-db-active-breakpoint-face-colors (cons "#1d2127" "#87ceeb"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1d2127" "#87875f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1d2127" "#686858"))
 '(objed-cursor-color "#aa4450")
 '(package-selected-packages
   '(god-mode lsp-ivy lsp-ui company-box which-key web-mode use-package ssass-mode smex smartparens php-mode org-download org-bullets nameframe-perspective multiple-cursors mmm-mode magit lsp-mode leuven-theme js2-mode js-doc iy-go-to-char ivy-rich helm-swoop helm-projectile helm-perspeen helm-ag groovy-mode goto-chg go-mode git-timemachine fira-code-mode expand-region elpy edit-indirect dumb-jump dtrt-indent drag-stuff doom-themes doom-modeline docker-compose-mode dired-subtree dired-git-info counsel-projectile company-ycmd company-web company-jedi company-anaconda centaur-tabs bm anzu ag ace-window))
 '(pdf-view-midnight-colors (cons "#c2c2b0" "#222222"))
 '(rustic-ansi-faces
   ["#222222" "#aa4450" "#87875f" "#cc8800" "#87AFD7" "#8787AF" "#87ceeb" "#c2c2b0"])
 '(vc-annotate-background "#222222")
 '(vc-annotate-color-map
   (list
    (cons 20 "#87875f")
    (cons 40 "#9e873f")
    (cons 60 "#b5871f")
    (cons 80 "#cc8800")
    (cons 100 "#dd8d00")
    (cons 120 "#ee9200")
    (cons 140 "#ff9800")
    (cons 160 "#d7923a")
    (cons 180 "#af8c74")
    (cons 200 "#8787AF")
    (cons 220 "#92708f")
    (cons 240 "#9e5a6f")
    (cons 260 "#aa4450")
    (cons 280 "#994d51")
    (cons 300 "#895654")
    (cons 320 "#785f55")
    (cons 340 "#62686E")
    (cons 360 "#62686E")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
