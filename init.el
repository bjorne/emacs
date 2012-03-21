;; debugging
;; (setq debug-on-error t)
;; (setq debug-on-entry t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; open init.el with f8
(global-set-key (kbd "<f8>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


