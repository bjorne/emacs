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
(package-initialize)

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

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)


;; colors!
(require 'color-theme-bjorne)
(color-theme-bjorne)

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
(use-package ruby-mode
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
                                                 (rspec-rerun))))))

        ;; (setq rspec-use-rake-when-possible nil)
        ;; (defadvice rspec-compile (around rspec-compile-around activate)
        ;;   "Use BASH shell for running the specs because of ZSH issues."
        ;;   (let ((shell-file-name "/bin/bash"))
        ;;     ad-do-it))
        )))
  :config
  (progn
    ;; (setq ruby-align-to-stmt-keywords '(begin if while unless until case for def))
    ;; (setq ruby-insert-encoding-magic-comment nil)
    ;; (setq ruby-deep-indent-paren nil))
  :bind (("C-M-h" . backward-kill-word)
         ("C-M-n" . scroll-up-five)
         ("C-M-p" . scroll-down-five))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))
(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
  :config (add-hook 'markdown-mode-hook 'bjorne-coding-hook))
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
    (define-key projectile-mode-map (kbd "C-c p g") 'ag-project)
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
    (set-face-background 'powerline-active1 "gray18")
    (set-face-background 'powerline-active2 "DarkGreen")
    (set-face-foreground 'powerline-active1 "white")
    (set-face-foreground 'powerline-active2 "white")
    (set-face-background 'mode-line "ForestGreen")
    (set-face-foreground 'mode-line "white")
    (set-face-attribute 'mode-line-inactive nil :box nil)
    (set-face-background 'powerline-inactive1 "gray22")
    (set-face-background 'powerline-inactive2 "gray11")
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
  :ensure t
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/Code"))
(use-package highlight2clipboard)
(use-package re-builder
  :config
  (progn
    (setq reb-re-syntax 'string)))
(use-package pcre2el
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
(setq whitespace-style (quote (face tabs empty trailing)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(ag-ignore-list (quote ("builds/" "*.min.js" "dist/")))
 '(coffee-args-repl (quote ("-i" "NODE_NO_READLINE=1")))
 '(css-indent-offset 2)
 '(ispell-program-name "aspell")
 '(js2-basic-offset 2)
 '(js2-mirror-mode t)
 '(json-reformat:indent-width 2)
 '(lintnode-port 3000)
 '(magit-rebase-arguments (quote ("--autosquash")))
 '(package-selected-packages
   (quote
    (dash magithub tide company smartparens nvm yasnippet-snippets apib-mode dockerfile-mode pcre2el bundler ruby-tools enh-ruby-mode eslint-fix highlight2clipboard ggtags avy magit-filenotify browse-at-remote copy-as-format flymd js-auto-beautify rjsx-mode gh-md typescript-mode counsel-projectile lorem-ipsum iedit rbenv ivy-hydra counsel ivy-bibtex flyspell-correct-ivy ivy zencoding-mode yasnippet yaml-mode wrap-region web-mode use-package textmate smex scratch rspec-mode restclient projectile prodigy powerline paredit pallet osx-dictionary multiple-cursors markdown-mode mark-multiple magit-gh-pulls json-mode js2-mode js-comint jade-mode imenu-anywhere idle-highlight-mode highlight-indentation haml-mode google-maps gist free-keys flycheck expand-region exec-path-from-shell evm evil ess drag-stuff discover color-theme coffee-mode ag actionscript-mode ace-jump-mode)))
 '(rspec-use-bundler-when-possible nil)
 '(send-mail-function (quote mailclient-send-it))
 '(typescript-indent-level 2)
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

