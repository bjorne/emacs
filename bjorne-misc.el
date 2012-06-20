(defalias 'yes-or-no-p 'y-or-n-p)

;; mouse scrolling ftw, not
(setq mouse-wheel-scroll-amount '(8 ((shift) . 8))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; cua mode
;; (cua-mode t)
(cua-selection-mode t)

;; linum colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :inverse-video t :height 0.8)))))


(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))

;; global modes
(wrap-region-global-mode 1)
(yas/global-mode 1)

(require 'mark-more-like-this)

;; unique buffer names
(require 'uniquify)

;; save cursor
(require 'saveplace)
(setq save-place t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory "~/.emacs.d/oddmuse"
      save-place-file "~/.emacs.d/places"
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      diff-switches "-u")

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
;; Highlight current line
(global-hl-line-mode 1)


;; ido-mode is like magic pixie dust!
(ido-mode t)
;; (ido-ubiquitous t) ?
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(setq smex-save-file (concat bjorne-root ".smex-items"))
(smex-initialize)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; pretty lambdas (esk)
(font-lock-add-keywords
 nil `(("(?\\(lambda\\>\\)"
	(0 (progn (compose-region (match-beginning 1) (match-end 1)
				  ,(make-char 'greek-iso8859-7 107))
		  nil)))))
;; highlight words
(font-lock-add-keywords
 nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
        1 font-lock-warning-face t)))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))
(require 'magit-blame)

;; from starter kit
;; Hippie expand: at times perhaps too hip
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))
;; Add this back in at the end of the list.
(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)


;; Javascript
(setq js-indent-level 2)
(eval-after-load 'js
  '(progn (define-key js-mode-map (kbd ",") 'self-insert-command) ;; fixes problem with pretty function font-lock
          (font-lock-add-keywords
           'js-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "\u0192")
                                 nil)))))))
(eval-after-load 'js2-mode
  '(progn (define-key js2-mode-map (kbd ",") 'self-insert-command) ;; fixes problem with pretty function font-lock
          (font-lock-add-keywords
           'js2-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "\u0192")
                                 nil)))))))

;; ruby interpolate string
(add-hook 'ruby-mode-hook (lambda () (global-set-key (kbd "#") 'ruby-interpolate)))

;; yasnippets
;;


;; projectile
(projectile-global-mode) ;; to enable in all buffers
(setq projectile-enable-caching t)
(add-to-list 'projectile-ignored-files ".DS_Store")
(add-to-list 'projectile-ignored-directories "node_modules")
;; coding hook
(defun bjorne-coding-hook ()
  (linum-mode)
  (idle-highlight-mode 1)
  (subword-mode)
  )

(add-hook 'js2-mode-hook 'bjorne-coding-hook)
(add-hook 'js-mode-hook 'bjorne-coding-hook)
(add-hook 'ruby-mode-hook 'bjorne-coding-hook)
(add-hook 'markdown-mode-hook 'bjorne-coding-hook)
(add-hook 'coffee-mode-hook 'bjorne-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'bjorne-coding-hook)

;; emacsclient ftw
(server-start)

(provide 'bjorne-misc)
