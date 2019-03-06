(when (string-equal system-type "darwin")
    (set-default-font "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"))

(global-auto-revert-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; cua mode
(cua-selection-mode t)

;; linum colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :inverse-video t :height 0.8)))))


(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq require-final-newline nil)
(defadvice basic-save-buffer (around fix-require-final-newline activate)
  (let* ((outer-buffer-file-name buffer-file-name)
         (require-final-newline
          (or
           (not (file-exists-p buffer-file-name))
           (with-temp-buffer (insert-file-contents outer-buffer-file-name) (= (char-after (1- (point-max))) ?\n)))))
    ad-do-it))

(setq
 visible-bell t
 inhibit-startup-message t
 color-theme-is-global t
 sentence-end-double-space nil
 shift-select-mode nil
 mouse-yank-at-point t
 uniquify-buffer-name-style 'forward
 whitespace-style '(face trailing lines-tail tabs)
 whitespace-line-column 80
 ediff-window-setup-function 'ediff-setup-windows-plain
 save-place-file (concat var-dir "places")
 backup-directory-alist `(("." . ,(expand-file-name (concat var-dir "backups"))))
 diff-switches "-u"
 recentf-save-file (concat var-dir "recentf"))
(setq tramp-default-method "ssh")

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
;; Highlight current line
(global-hl-line-mode 1)

;; (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

;; (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;; (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
;; (add-hook 'ido-setup-hook 'ido-define-keys)

(setq smex-save-file (concat var-dir ".smex-items"))
(smex-initialize)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

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

;; (eval-after-load 'diff-mode
;;   '(progn
;;      (set-face-foreground 'diff-added "green4")
;;      (set-face-foreground 'diff-removed "red3")))

;; from starter kit
;; Hippie expand: at times perhaps too hip
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially try-complete-lisp-symbol))
  (delete f hippie-expand-try-functions-list))
;; Add this back in at the end of the list.
(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)

;; emacsclient ftw
(server-start)

;; dont ping machines ffs
(setq ffap-machine-p-known 'reject)

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (R . t)
        (ruby . t)
        (perl . t)
        (python .t)))

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

(provide 'bjorne-misc)
