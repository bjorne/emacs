;; debugging
;; (setq debug-on-error t)
;; (setq debug-on-entry t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PATH
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; Start in $HOME
(setq default-directory (getenv "HOME"))

;; root dir
(setq bjorne-root "~/.emacs.d/")
(add-to-list 'load-path (concat bjorne-root "lisp/"))
(add-to-list 'load-path (concat bjorne-root "vendor/"))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

;; well i got hacked one day...
;; (add-hook 'find-file-hook 'zone)
;; (setq counter 0)
;; (add-hook 'find-file-hook (lambda () (when (= (% counter 50) 0) (zone))))

;; melpa setup
;; (require 'package)
;; (package-initialize)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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
 '(magit-diff-refine-hunk t)
 '(magit-highlight-indentation nil)
 '(magit-highlight-trailing-whitespace t)
 '(magit-process-popup-time 1)
 '(magit-repo-dirs (quote ("~/Code")))
 '(magit-repo-dirs-depth 2)
 '(magit-set-upstream-on-push t)
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(send-mail-function (quote mailclient-send-it))
 '(yas-prompt-functions
   (quote
    (yas/ido-prompt yas/x-prompt yas/dropdown-prompt yas/completing-prompt yas/ido-prompt yas/no-prompt)))
 '(yas/trigger-key nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :inverse-video t :height 0.8))))
 '(secondary-selection ((t (:background "#273737")))))
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
