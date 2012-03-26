(global-set-key (kbd "<f10>") 'kill-buffer)
(global-set-key (kbd "<f11>") 'kill-buffer-and-window)

(require 'ace-jump-mode)
(global-set-key (kbd "<f5>") 'ace-jump-mode)
(global-set-key (kbd "<f6>") 'ace-jump-word-mode)
(global-set-key (kbd "<f7>") 'ace-jump-line-mode)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c q") 'join-line)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning-of-line)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer-in-project)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "<f8>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x C-g") 'keyboard-quit)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c g") 'magit-status)
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

;; Switch the Cmd and Meta keys
(set-keyboard-coding-system nil)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(windmove-default-keybindings) ;; Shift+direction

(provide 'bjorne-bindings)
