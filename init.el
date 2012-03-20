;; debugging
(setq debug-on-error t)
(setq debug-on-entry t)

;; root dir
(add-to-list 'load-path "~/.emacs.d")

;; el-get setup and install (if not)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el") (end-of-buffer) (eval-print-last-sexp)))

;; define which el-get packages we wantz
;; do not install via GUI, do it here instead
(setq my-packages
      '(color-theme
	el-get
	google-maps
	highlight-indentation
	magit
	markdown-mode
	mo-git-blame
	rspec-mode
	rvm
	textmate
	yaml-mode))

;; install packages listed above
(el-get 'sync (append
       my-packages
       (mapcar 'el-get-source-name el-get-sources)))

;; colors!
(require 'color-theme-bjorne)
(color-theme-bjorne)

;; open init.el with f8
(global-set-key (kbd "<f8>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
