;; debugging
;; (setq debug-on-error t)
;; (setq debug-on-entry t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PATH
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; root dir
(setq bjorne-root "~/.emacs.d/")
(add-to-list 'load-path bjorne-root)
(add-to-list 'load-path (concat bjorne-root "vendor/"))
(add-to-list 'load-path (concat bjorne-root "el-get/el-get/"))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

;; melpa setup
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; el-get setup and install (if not)
(setq el-get-install-skip-emacswiki-recipes t)
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-sources '(
;;                        (:name lintnode
;;                               :type github
;;                               :pkgname "davidmiller/lintnode")
                       (:name ess
                              :description "Emacs Speaks Statistics: statistical programming within Emacs"
                              :type github
                              :pkgname "emacs-ess/ESS"
                              :info "doc/info/"
                              :build (\` (\, (mapcar (lambda (target) (concat "make " target " EMACS=" el-get-emacs)) (quote ("clean" "all")))))
                              :load-path ("lisp")
                              :features ess-site)))

;; define which el-get packages we wantz
;; do not install via GUI, do it here instead
(setq my-packages
      '(ace-jump-mode
        ;; ack-and-a-half
        coffee-mode
        color-theme
        drag-stuff
	el-get
        ess
        expand-region
	google-maps
        haml-mode
;;        helm
	highlight-indentation
        idle-highlight-mode
        js-comint
        js2-mode
;;        lintnode
	magit
        mark-multiple
	markdown-mode
	mo-git-blame
;;        nxhtml
        paredit
;;        popwin
        projectile
	rspec-mode
	rvm
        smex
	s
	textmate
        wrap-region
	yaml-mode
        yasnippet
        zencoding-mode))

(setq el-get-user-package-directory "~/.emacs.d/el-get-init")

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

(setq whitespace-style (quote (face tabs empty trailing)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-args-repl (quote ("-i" "NODE_NO_READLINE=1")))
 '(css-indent-offset 2)
 '(ispell-program-name "aspell")
 '(js2-basic-offset 2)
 '(js2-mirror-mode t)
 '(lintnode-port 3000)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(magit-highlight-indentation nil)
 '(magit-highlight-trailing-whitespace t)
 '(magit-process-popup-time 1)
 '(magit-repo-dirs (quote ("~/Code")))
 '(magit-repo-dirs-depth 2)
 '(magit-set-upstream-on-push t)
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(send-mail-function (quote mailclient-send-it))
 '(yas-prompt-functions (quote (yas/ido-prompt yas/x-prompt yas/dropdown-prompt yas/completing-prompt yas/ido-prompt yas/no-prompt)))
 '(yas/trigger-key nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :inverse-video t :height 0.8)))))
(put 'narrow-to-region 'disabled nil)
