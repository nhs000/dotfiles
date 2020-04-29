(package-initialize)

(require 'org)
(require 'ob-tangle)

(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))
(org-babel-load-file (expand-file-name "loader.org" init-dir))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-subtree-depth-1-face ((t (:background "white smoke"))))
 '(dired-subtree-depth-2-face ((t (:background "gainsboro"))))
 '(dired-subtree-depth-3-face ((t (:background "light gray"))))
 '(dired-subtree-depth-4-face ((t (:background "light gray"))))
 '(header-line ((t (:background "#ff6e68" :foreground "#4901f8" :box nil :overline nil :underline nil)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(helm-rg smartparens helm-perspeen nameframe-perspective perspective php-mode leuven-theme phps-mode helm-swoop org-download centaur-tabs iy-go-to-char dumb-jump dtrt-indent expand-region multiple-cursors git-timemachine ace-window goto-chg magit company bm flycheck anzu helm-projectile helm ag projectile org-bullets "use-package" use-package "use-package")))
