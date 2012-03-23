;; debugging
;; (setq debug-on-error t)
;; (setq debug-on-entry t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'yes-or-no-p 'y-or-n-p)

;; root dir
(add-to-list 'load-path "~/.emacs.d")

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EL-GET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	rspec-mode
	rvm
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f10>") 'kill-buffer)
(global-set-key (kbd "<f11>") 'kill-buffer-and-window)

(require 'ace-jump-mode)
(global-set-key (kbd "<f5>") 'ace-jump-mode)
(global-set-key (kbd "<f6>") 'ace-jump-word-mode)
(global-set-key (kbd "<f7>") 'ace-jump-line-mode)

(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning-of-line)

;; ruby interpolate string
(add-hook 'ruby-mode-hook (lambda () (global-set-key (kbd "#") 'ruby-interpolate)))

(global-set-key (kbd "C-x C-b") 'switch-to-buffer-in-project)

;; open init.el with f8
(global-set-key (kbd "<f8>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; alias keyboard-quit
(global-set-key (kbd "C-x C-g") 'keyboard-quit)

;; Switch the Cmd and Meta keys
(set-keyboard-coding-system nil)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

(windmove-default-keybindings) ;; Shift+direction

;; M-S-6 is awkward
(global-set-key (kbd "C-c q") 'join-line)

;; So good!
(global-set-key (kbd "C-c g") 'magit-status)

;; mark multiple
(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; wrap-region
(wrap-region-global-mode 1)

;; unique buffer names
(require 'uniquify)

(yas/global-mode 1)

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


;; colors!
(require 'color-theme-bjorne)
(color-theme-bjorne)

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;; Javascript
(setq js-indent-level 2)
(eval-after-load 'js
  '(progn (define-key js-mode-map (kbd ",") 'self-insert-command) ;; fixes problem with pretty function font-lock
          (font-lock-add-keywords
           'js-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "\u0192")
                                 nil)))))))
