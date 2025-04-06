(require 'org)
(org-babel-load-file
 (expand-file-name "loader.org"
                   user-emacs-directory))
(put 'narrow-to-region 'disabled nil)
