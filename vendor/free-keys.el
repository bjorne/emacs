(setq free-keys-modifiers (list "C" "M" "C-M"))
(setq free-keys-keys "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.,/§1234567890-=[];'\\`±!@#$%^&*()_+}{:\"|?><~")

(let ((buf (get-buffer-create "*Free keys*")))
    (display-buffer buf))

(defun free-keys ()
  (interactive)
  (let ((buf (get-buffer-create "*Free keys*")))
    (display-buffer buf)
    (with-current-buffer buf
      (erase-buffer)
      (mapc (lambda (modifier)
              (insert "\nFree keys with modifier " modifier "\n=========================\n")
              (mapc (lambda (key)
                      (let* ((full-name
                              (concat modifier "-" (char-to-string key)))
                             (binding
                              (key-binding (read-kbd-macro full-name))))
                        (when (not binding)
                          (insert
                           full-name
                           " maps to "
                           (symbol-name binding)
                           "\n"))))
                    free-keys-keys))
            free-keys-modifiers)
      (setq buffer-read-only t)
      (make-local-variable 'buffer-read-only)
      (goto-char 0))))

(provide 'free-keys)
