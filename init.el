;; debugging
;; (setq debug-on-error t)
;; (setq debug-on-entry t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; root dir
(setq bjorne-root "~/.emacs.d/")
(add-to-list 'load-path bjorne-root)

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

;; el-get setup and install (if not)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (lambda (s) (let (el-get-master-branch) (end-of-buffer) (eval-print-last-sexp)))))

;; define which el-get packages we wantz
;; do not install via GUI, do it here instead
(setq my-packages
      '(ace-jump-mode
        color-theme
	el-get
	google-maps
	highlight-indentation
	magit
        mark-multiple
	markdown-mode
	mo-git-blame
        nxhtml
        paredit
	rspec-mode
	rvm
        smex
	textmate
        wrap-region
	yaml-mode
        yasnippet
        zencoding-mode))

;; install packages listed above
(el-get 'sync (append
       my-packages
       (mapcar 'el-get-source-name el-get-sources)))

(require 'bjorne-defuns)
(require 'bjorne-misc)
(require 'bjorne-bindings)

;; colors!
(require 'color-theme-bjorne)
(color-theme-bjorne)
