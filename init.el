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
(setq var-dir (concat bjorne-root "var/"))
(add-to-list 'load-path (concat bjorne-root "lisp/"))
(add-to-list 'load-path (concat bjorne-root "vendor/"))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(require 'bjorne-defuns)
(require 'bjorne-misc)
(require 'bjorne-bindings)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package no-littering)

(use-package all-the-icons)
(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defun bjorne-coding-hook ()
  "Set things up for coding."
  (auto-fill-mode 0)
  (linum-mode)
  (idle-highlight-mode 1)
  (subword-mode)
  (whitespace-mode))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind (("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word))
  :config
  (progn
    (add-hook 'js2-mode-hook 'bjorne-coding-hook)
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (progn (define-key js2-mode-map (kbd ",") 'self-insert-command) ;; fixes problem with pretty function font-lock
           (font-lock-add-keywords
            'js2-mode `(("\\(function *\\)("
                         (0 (progn (compose-region (match-beginning 1)
                                                   (match-end 1) "\u0192")
                                   nil))))))))

(use-package json-mode
  :mode (("\\.json" . json-mode))
  :config (add-hook 'json-mode-hook 'bjorne-coding-hook))
(use-package coffee-mode
  :config
  (progn
    (add-hook 'coffee-mode-hook 'bjorne-coding-hook)
    (add-hook 'coffee-mode-hook
              (lambda ()
                (bind-key "C-j" 'coffee-newline-and-indent coffee-mode-map)
                (setq coffee-tab-width 2)))))
(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
	 ("\\.json\\'" . web-mode)
;;         ("\\.erb\\'"  . web-mode)
         ("\\.html?\\'"  . web-mode))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-enable-css-colorization t)
                        (setq web-mode-markup-indent-offset 2)
                        (setq web-mode-style-padding 2)
                        (setq web-mode-script-padding 2)))
            (flycheck-add-mode 'javascript-eslint 'web-mode))
  :commands web-mode
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

  (setq web-mode-content-types-alist
	'(("jsx" . "\\.js[x]?\\'"))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  

  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :config
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  :custom (tide-tsserver-process-environment)
  :hook (web-mode . (lambda ()
            (when (string-match-p "jsx?\\|tsx?" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
  )

(use-package prettier
  :init (global-prettier-mode))

(use-package enh-ruby-mode
  :ensure t
  :init
  (progn
    ;; (use-package ruby-tools)
    (use-package bundler)
    (use-package rspec-mode
      :config
      (progn
        (add-hook 'ruby-mode-hook 'bjorne-coding-hook)
        (add-hook 'compilation-mode-hook
                  (lambda ()
                    (when (eq major-mode 'rspec-compilation-mode)
                      (setq compilation-scroll-output t)
                      (local-set-key (kbd "g") (lambda ()
                                                 (interactive)
                                                 (rspec-rerun)))))))))
  :config
  (progn
    (setq enh-ruby-deep-indent-paren nil)
    (setq enh-ruby-deep-indent-construct nil)
    (setq enh-ruby-add-encoding-comment-on-save nil)
    (add-hook 'enh-ruby-mode-hook 'bjorne-coding-hook)
    (add-to-list 'auto-mode-alist
                 '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))))
(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
  :config
  (progn
    (add-hook 'markdown-mode-hook 'bjorne-coding-hook)
    (add-hook 'markdown-mode-hook 'visual-line-mode)))
(use-package flycheck
  :init
  (setq flycheck-disabled-checkers '(ruby-rubocop ruby-rubylint))
  :config
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)))
(use-package multiple-cursors
  :config
  (setq mc/list-file (concat var-dir "mc-lists.el")))
;; (use-package saveplace
;;   :init
;;   (setq save-place t))
(use-package copy-as-format)
(use-package browse-at-remote
  :config
  (setq browse-at-remote-prefer-symbolic nil)
  (global-set-key (kbd "C-c n") 'browse-at-remote))

(defun bjorne/counsel-yank-or-yank-pop (&optional arg)
  "Call `counsel-yank-pop'. If called after a yank, call `yank-pop' instead."
  (interactive "*p")
  (if (eq last-command 'yank)
      (yank-pop arg)
    (counsel-yank-pop)))
(use-package counsel
  :demand t)
(use-package ivy
  :config
  (progn
    (ivy-mode 1)
    (defun ivy-magit-dir (x)
      (print (concat "dir" (magit-toplevel (if (f-dir-p x) x (f-dirname x)))))
      (with-ivy-window
       (magit-status-internal (magit-toplevel (if (f-dir-p x) x (f-dirname x))))))

    (require 'ivy)
    (ivy-set-actions
     'dired
     '(("g" ivy-magit-dir "magit")))
    (ivy-set-actions
     'counsel-find-file
     '(("g" ivy-magit-dir "magit")))
    (setq ivy-use-virtual-buffers t)
    (setq ivy-extra-directories nil)
    (setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x b") 'switch-to-buffer)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c a") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (global-set-key (kbd "M-y") 'bjorne/counsel-yank-or-yank-pop)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))
(use-package ag
  ;; :config
  ;; (setq ag-arguments '("--smart-case" "--nogroup" "--ignore-dir=node_modules" "--ignore-dir=.cask" "--ignore-dir=backups" "--ignore-dir=tmp" "--ignore=projectile.cache" "--ignore-dir=coverage" "--ignore-dir=public/assets" "--"))
  )
(use-package projectile
  :init (projectile-global-mode 1)
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ivy)
    (setq projectile-cache-file (concat var-dir "projectile.cache"))
    (setq projectile-known-projects-file (concat var-dir "projectile-bookmarks.eld"))
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-globally-ignored-files "*.min.js*")
    (add-to-list 'projectile-globally-ignored-files "*.bundle.js")
    (add-to-list 'projectile-globally-ignored-directories "dist")
    (add-to-list 'projectile-globally-ignored-directories ".bloop"))
  :bind (:map projectile-mode-map
              ("C-c p g" . ag-project)))
(use-package counsel-projectile
  :ensure t
  :init (counsel-projectile-mode))
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (progn
    (setq magit-completing-read-function 'ivy-completing-read)
    (setq magit-branch-arguments nil)
    (setq magit-process-popup-time 1)
    (setq magit-diff-refine-hunk t)
    )
  :bind
  (("C-c g" . magit-status)
   ("C-c b" . magit-blame)))
(use-package re-builder
  :config
  (progn
    (setq reb-re-syntax 'string)))
(use-package pcre2el
  :ensure t)
(use-package ess
  :ensure t)
(use-package dockerfile-mode
  :ensure t
  :mode (("^Dockerfile" . dockerfile-mode)))
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs (list (concat bjorne-root "snippets")))
  :config
  (yas-global-mode 1)
  (yas-reload-all))
(use-package yasnippet-snippets
  :ensure t)
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package python-mode
  :ensure nil
  :commands python-mode
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode t)
              (setq tab-width 4))))

(use-package lsp-mode
  :ensure t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-prefer-flymake nil)
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode :ensure t)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol :ensure t)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  :ensure t
  )
;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :ensure t
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :ensure t
  :custom
  (lsp-metals-show-implicit-arguments t)
  (lsp-metals-show-inferred-type t)
  ;; :config (setq lsp-metals-treeview-show-when-views-received t)
  )

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook
  (eshell-first-time-mode . efs/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  (eshell-git-prompt-use-theme 'robbyrussell)
  :bind
  ([remap eshell-previous-matching-input] . counsel-esh-history))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000)
  (setq vterm-buffer-name-string "vt %s"))


(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config (setq dired-dwim-target t))

  ;; :custom ((dired-listing-switches "-al --group-directories-first")))

(use-package dired-single
  :bind
  (:map dired-mode-map (
                        ([remap dired-find-file] . dired-single-buffer)
                        ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
                        ([remap dired-up-directory] . dired-single-up-directory))))


(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


(use-package browse-at-remote
  :custom
  (browse-at-remote-remote-type-domains '(("bitbucket.org" . "bitbucket")
                                         ("github.com" . "github")
                                         ("gitlab.com" . "gitlab")
                                         ("git.savannah.gnu.org" . "gnu")
                                         ("gist.github.com" . "gist")
                                         ("ghe.spotify.net" . "github"))))

(use-package browse-kill-ring)

(use-package yaml-mode
  :mode (("\\.ya?ml" . yaml-mode)))

;; Font and style

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; set a default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-16"))

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))


;; Configure and load customizations

(setq custom-file (expand-file-name "custom.el" var-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
