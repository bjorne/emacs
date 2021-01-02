;; debugging
;; (setq debug-on-error t)
;; (setq debug-on-entry t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PATH

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; Start in $HOME
(setq default-directory (getenv "HOME"))

;; root dir
(setq bjorne-root "~/.emacs.d/")
(setq var-dir (concat bjorne-root "var/"))
(add-to-list 'load-path (concat bjorne-root "lisp/"))
(add-to-list 'load-path (concat bjorne-root "vendor/"))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(require 'cask (concat bjorne-root "vendor/cask/cask.el"))
(cask-initialize)
(require 'pallet)
(pallet-mode t)


;; colors!
; (require 'color-theme-bjorne)
; (color-theme-bjorne)

(require 'bjorne-defuns)
(require 'bjorne-misc)
(require 'bjorne-bindings)

(require 'use-package)

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

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
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-enable-css-colorization t)
                        (setq web-mode-markup-indent-offset 2)
                        (setq web-mode-style-padding 2)
                        (setq web-mode-script-padding 2)))))

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
    (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook 'global-flycheck-mode)))
(use-package multiple-cursors
  :config
  (setq mc/list-file (concat var-dir "mc-lists.el")))
(use-package uniquify)
(use-package saveplace
  :init
  (setq save-place t))
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))
(use-package copy-as-format)
(use-package browse-at-remote
  :config
  (setq browse-at-remote-prefer-symbolic nil)
  (global-set-key (kbd "C-c n") 'browse-at-remote))
(use-package counsel
  :demand t)
(use-package ivy
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-extra-directories nil)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-i") 'ivy-imenu-anywhere)
    (global-set-key (kbd "M-x") 'counsel-M-x)
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
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))
(use-package ag
  ;; :config
  ;; (setq ag-arguments '("--smart-case" "--nogroup" "--ignore-dir=node_modules" "--ignore-dir=.cask" "--ignore-dir=backups" "--ignore-dir=tmp" "--ignore=projectile.cache" "--ignore-dir=coverage" "--ignore-dir=public/assets" "--"))
  )
(use-package projectile
  :init (projectile-global-mode 1)
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p t") 'projectile-toggle-test-code)
    ;; (define-key projectile-mode-map (kbd "C-c p g") 'ag-project)
    ;; (define-key projectile-mode-map (kbd "C-c p f") 'projectile-find-file)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
    (setq projectile-enable-caching t) 
    (setq projectile-completion-system 'ivy)
    (setq projectile-cache-file (concat var-dir "projectile.cache"))
    (setq projectile-known-projects-file (concat var-dir "projectile-bookmarks.eld"))
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-globally-ignored-files "*.min.js*")
    (add-to-list 'projectile-globally-ignored-files "*.bundle.js")
    (add-to-list 'projectile-globally-ignored-files "dist/"))
  :bind (:map projectile-mode-map
              ("C-c p t" . projectile-toggle-test-code)
              ("C-c p g" . ag-project)))
(use-package counsel-projectile
  :ensure t
  :init (counsel-projectile-mode))
(use-package powerline
   :config
   (progn
;;     (set-face-background 'powerline-active1 "gray18")
     ;; (set-face-background 'powerline-active2 "DarkGreen")
;;     (set-face-foreground 'powerline-active1 "white")
;;     (set-face-foreground 'powerline-active2 "white")
     (set-face-background 'mode-line "ForestGreen")
;;     (set-face-foreground 'mode-line "white")
;;     (set-face-attribute 'mode-line-inactive nil :box nil)
;;     (set-face-background 'powerline-inactive1 "gray22")
;;     (set-face-background 'powerline-inactive2 "gray11")
     (powerline-default-theme)))
(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map))
  :config
  (progn
    (setq magit-completing-read-function 'ivy-completing-read)
    (setq magit-repository-directories '("~/Code"))
    (setq magit-branch-arguments nil)
    (setq magit-process-popup-time 1)
    (setq magit-diff-refine-hunk t)
    (magit-define-popup-action 'magit-rebase-popup
      ?M "master\n" (lambda ()
                      (interactive)
                      (magit-rebase "master" (magit-rebase-arguments))) ?e t)
    (magit-define-popup-action 'magit-rebase-popup
      ?o "origin/master\n" (lambda ()
                      (interactive)
                      (magit-rebase "origin/master" (magit-rebase-arguments))) ?M))
  :bind
  (("C-c g" . magit-status)
   ("C-c b" . magit-blame)))
(use-package magithub
  :disabled t
  :ensure t
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-cache-file (concat var-dir "magithub/cache"))
  (setq magithub-clone-default-directory "~/Code"))
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
;; (use-package solarized-theme
;;   :ensure t
;;   :defer t
;;   :init (load-theme 'solarized-dark t))
;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t)
;; ;; (use-package badger-theme
;; ;;   :ensure t
;; ;;   :config (load-theme 'badger t))
;; (use-package alect-themes
;;   :ensure t
;;   :defer t
;;   :init (load-theme 'alect-dark t))
(use-package darcula-theme
  :ensure t)
;;  :init (load-theme 'darcula))
(use-package tide
  :ensure t
  :config (progn
            (setq company-tooltip-align-annotations t)
            ;; (add-hook 'before-save-hook 'tide-format-before-save)
            (add-hook 'typescript-mode-hook (lambda ()
                                              (interactive)
                                              (tide-setup)
                                              (flycheck-mode +1)
                                              (eldoc-mode +1)
                                              (tide-hl-identifier-mode +1)
                                              (company-mode +1)))))

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

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
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol :ensure t)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol :ensure t)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list :ensure t)

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
  :ensure t
  :config
  (which-key-mode))

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
  :config (setq lsp-metals-treeview-show-when-views-received t))

(setq whitespace-style (quote (face tabs empty trailing)))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(
;;    (sh . t)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(ag-ignore-list '("builds/" "*.min.js" "dist/"))
 '(coffee-args-repl '("-i" "NODE_NO_READLINE=1"))
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("675c0a75d42b42ed963e426b3d8582ec719742b9033179b8d8fef89fae0dfbff" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(ispell-program-name "aspell")
 '(js2-basic-offset 2)
 '(js2-mirror-mode t)
 '(json-reformat:indent-width 2)
 '(lintnode-port 3000)
 '(magit-rebase-arguments '("--autosquash"))
 '(package-selected-packages
   '(lsp-metals sbt-mode which-key dap-mode lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode ess darcula-theme alect-themes color-theme-sanityinc-tomorrow solarized-theme badger-theme spacemacs-theme scala-mode jq-mode graphql-mode vue-mode esup ansible markdown-preview-mode dash magithub tide company smartparens nvm yasnippet-snippets apib-mode dockerfile-mode pcre2el bundler ruby-tools enh-ruby-mode eslint-fix ggtags avy magit-filenotify browse-at-remote copy-as-format flymd js-auto-beautify rjsx-mode gh-md typescript-mode counsel-projectile lorem-ipsum iedit rbenv ivy-hydra counsel ivy-bibtex flyspell-correct-ivy ivy yasnippet yaml-mode wrap-region web-mode use-package textmate smex scratch rspec-mode restclient projectile prodigy powerline paredit pallet osx-dictionary multiple-cursors markdown-mode mark-multiple magit-gh-pulls json-mode js2-mode js-comint jade-mode imenu-anywhere idle-highlight-mode highlight-indentation haml-mode google-maps gist free-keys flycheck expand-region exec-path-from-shell evm evil drag-stuff discover coffee-mode ag actionscript-mode ace-jump-mode))
 '(rspec-use-bundler-when-possible nil)
 '(send-mail-function 'mailclient-send-it)
 '(typescript-indent-level 2)
 '(vue-html-extra-indent 2)
 '(web-mode-code-indent-offset 2)
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

