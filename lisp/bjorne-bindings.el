(global-set-key
 (kbd "C-x C-c")
 (lambda ()
   (interactive)
   (if (y-or-n-p "Quit Emacs? ")
       (save-buffers-kill-emacs))))

(global-unset-key (kbd "C-x C-z"))

;; drag stuff
(global-set-key (kbd "M-n") 'drag-stuff-down)
(global-set-key (kbd "M-p") 'drag-stuff-up)

(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-p") 'previous-line)

(global-set-key (kbd "C-<f7>") (lambda () (interactive) (window-configuration-to-register ?1)))
(global-set-key (kbd "<f7>") (lambda () (interactive) (jump-to-register ?1)))
(global-set-key (kbd "C-<f8>") (lambda () (interactive) (window-configuration-to-register ?2)))
(global-set-key (kbd "<f8>") (lambda () (interactive) (jump-to-register ?2)))
;; (global-set-key (kbd "C-x p") popwin:keymap)

(global-set-key (kbd "<f10>") 'kill-buffer)
(global-set-key (kbd "<f11>") 'kill-buffer-and-window)

;; (require 'ace-jump-mode)
;;(global-set-key (kbd "M-h") 'ace-jump-mode)
(global-set-key (kbd "C-c q") 'join-line)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning-of-line)
(global-set-key (kbd "C-s-f") 'cycle-files-with-same-suffix)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)
(global-set-key (kbd "M-/") 'hippie-expand)
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
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-'") 'toggle-quotes)

(global-set-key (kbd "§") 'yas/expand)
(global-set-key (kbd "C-=") 'expand-region)

(global-set-key (kbd "s-/") 'comment-or-uncomment-current-line-or-region)

;; custom commands
(global-set-key (kbd "C-c e s") 'scratch)

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
