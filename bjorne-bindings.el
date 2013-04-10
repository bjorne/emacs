(global-unset-key (kbd "C-x C-c"))

(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

;; drag stuff
(global-set-key (kbd "M-n") 'drag-stuff-down)
(global-set-key (kbd "M-p") 'drag-stuff-up)


(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-p") 'previous-line)
(global-set-key (kbd "C-S-n") (lambda () (interactive) (next-line) (scroll-up 1)))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (previous-line) (scroll-down 1)))
(global-set-key (kbd "M-N") (lambda () (interactive) (next-line 5) (scroll-up 5)))
(global-set-key (kbd "M-P") (lambda () (interactive) (previous-line 5) (scroll-down 5)))

(global-set-key (kbd "C-<f7>") (lambda () (interactive) (window-configuration-to-register ?1)))
(global-set-key (kbd "<f7>") (lambda () (interactive) (jump-to-register ?1)))
(global-set-key (kbd "C-<f8>") (lambda () (interactive) (window-configuration-to-register ?2)))
(global-set-key (kbd "<f8>") (lambda () (interactive) (jump-to-register ?2)))
;; (global-set-key (kbd "C-x p") popwin:keymap)

(global-set-key (kbd "<f10>") 'kill-buffer)
(global-set-key (kbd "<f11>") 'kill-buffer-and-window)

(require 'ace-jump-mode)
(global-set-key (kbd "<f5>") 'ace-jump-mode)
;; (global-set-key (kbd "<f6>") 'ace-jump-word-mode)
;; (global-set-key (kbd "<f7>") 'ace-jump-line-mode)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c q") 'join-line)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning-of-line)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer-in-project)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "<f8>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "<f9>") (lambda () (interactive) (open-project "js_agent")))
(global-set-key (kbd "<f12>") 'textmate-goto-file)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-blame-mode)
(global-set-key (kbd "C-z") 'undo)
;; zoom
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; mark multiple
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-'") 'toggle-quotes)

(global-set-key (kbd "§") 'yas/expand)
(global-set-key (kbd "C-=") 'expand-region)

;; Switch the Cmd and Meta keys
(set-keyboard-coding-system nil)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(windmove-default-keybindings) ;; Shift+direction

(defun other-window-backwards ()
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-backwards)

(global-set-key (kbd "C-<tab>") 'indent-as-previous)

(provide 'bjorne-bindings)
