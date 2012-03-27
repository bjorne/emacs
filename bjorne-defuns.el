(defun project-root-find (&optional dir)
  "Finds project root from DIR."
  (when dir
    (or dir (setq dir default-directory))
    (if (file-exists-p (expand-file-name ".git" dir))
        (file-truename dir)
      (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
        (unless (equal dir "/")
          (project-root-find new-dir))))))

(defun switch-to-buffer-in-project ()
  "Switch to buffer in project."
  (interactive)
  (let* ((project (project-root-find (buffer-file-name (current-buffer))))
         (buffers
          (if project
              (delete-if-not
               (lambda (buffer)
                 (equal (project-root-find (buffer-file-name buffer)) project))
               (buffer-list))
            (buffer-list)))
         (buffer-names (mapcar 'buffer-name buffers)))
    (flet ((ido-make-buffer-list (default) buffer-names))
      (call-interactively 'ido-switch-buffer))))

(defun ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(defun back-to-indentation-or-beginning-of-line ()
  "Moves point back to indentation if there is any
non blank characters to the left of the cursor.
Otherwise point moves to beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun close-some-windows (numbers)
  "DOCSTRING"
  (interactive "sWindow numbers: ")
  (if (string-match "\\`[0-9]+\\'" numbers)
      (let ((numbers (string-to-list numbers)))
        (progn 
          (print numbers)
          (if (> (length (window-list)) 1)
              (dolist (num numbers) (delete-window (nth num (window-list))))
            (print (window-list))
            )))
    (print "Need a string of numbers.")))

(defun toggle-quotes ()
  "Change surrounding double quotes to single quotes, or vice versa.

Known bugs: Single quoted string within double quoted string, or
vice versa, will not work."
  (interactive)
  (save-excursion
    (when (and
           t
           (looking-back "\[\"\'\].+" (line-beginning-position))
           (looking-at ".+\[\"\'\]"))
      (save-excursion
        (re-search-backward "\[^\\\]\[\"\'\]")
        (forward-char)
        (let ((replacement
               (if (string-equal (char-to-string (char-after)) "\'") "\"" "\'")))
          (delete-char 1)
          (insert replacement)
          (forward-char)
          (re-search-forward "\[^\\\]\[\"\'\]")
          (delete-char -1)
          (insert replacement))))))

(provide 'bjorne-defuns)
