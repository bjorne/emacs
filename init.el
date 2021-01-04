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
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
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
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-i") 'ivy-imenu-anywhere)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
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
    (add-to-list 'projectile-globally-ignored-files "dist/"))
  :bind (:map projectile-mode-map
              ("C-c p g" . ag-project)))
(use-package counsel-projectile
  :ensure t
  :init (counsel-projectile-mode))
(use-package magit
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
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol :ensure t)

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
  ;; :config (setq lsp-metals-treeview-show-when-views-received t)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(ag-ignore-list '("builds/" "*.min.js" "dist/"))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(coffee-args-repl '("-i" "NODE_NO_READLINE=1"))
 '(compilation-message-face 'default)
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(fci-rule-color "#073642")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#3b6b40f432d6" "#07b9463c4d36" "#47a3341e358a" "#1d873c3f56d5" "#2d86441c3361" "#43b7362d3199" "#061d417f59d7"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(ispell-program-name "aspell")
 '(js-indent-level 2)
 '(js2-mirror-mode t)
 '(json-reformat:indent-width 2)
 '(lintnode-port 3000)
 '(lsp-metals-show-implicit-arguments t)
 '(lsp-metals-show-inferred-type t)
 '(lsp-ui-doc-border "#93a1a1")
 '(magit-rebase-arguments '("--autosquash"))
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
   '(lsp-metals sbt-mode scala-mode which-key dap-mode posframe lsp-ivy lsp-ui lsp-mode tide yasnippet-snippets yasnippet dockerfile-mode ess pcre2el magit counsel-projectile projectile ag ivy-prescient ivy-rich counsel browse-at-remote copy-as-format multiple-cursors flycheck markdown-mode rspec-mode bundler enh-ruby-mode web-mode coffee-mode json-mode js2-mode doom-themes doom-modeline all-the-icons exec-path-from-shell use-package))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(rspec-use-bundler-when-possible nil)
 '(send-mail-function 'mailclient-send-it)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(typescript-indent-level 2)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(vue-html-extra-indent 2)
 '(web-mode-code-indent-offset 2)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"])
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

(setq dired-dwim-target t)
