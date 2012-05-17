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
      (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
		    (lambda (s)
		      (let (el-get-master-branch)
			(end-of-buffer)
			(eval-print-last-sexp))))))

(setq el-get-sources '((:name idle-highlight-mode
                              :type github
                              :pkgname "nonsequitur/idle-highlight-mode")))

;; define which el-get packages we wantz
;; do not install via GUI, do it here instead
(setq my-packages
      '(ace-jump-mode
        coffee-mode
        color-theme
        drag-stuff
	el-get
	google-maps
	highlight-indentation
        idle-highlight-mode
	magit
        mark-multiple
	markdown-mode
	mo-git-blame
        nxhtml
        paredit
        popwin
        projectile
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(magit-highlight-indentation nil)
 '(magit-highlight-trailing-whitespace t)
 '(magit-process-popup-time 1)
 '(magit-repo-dirs (quote ("~/Code")))
 '(magit-repo-dirs-depth 2)
 '(magit-set-upstream-on-push t)
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(safe-global-variable-values (quote ((eval ignore-errors "Write-contents-functions is a buffer-global alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t))))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :inverse-video t :height 0.8)))))
