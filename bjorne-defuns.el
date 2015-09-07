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
  "Change surrounding double quotes to single quotes, or vice versa."
  (interactive)
  (save-excursion
    (when (and
           (looking-back "\\(\[\"\'\]\\).+" (line-beginning-position))
           (looking-at (format ".+\\(%s\\)" (match-string-no-properties 1))))
      (let* ((original (match-string-no-properties 1))
             (replacement (if (string-equal original "\'") "\"" "\'")))
        (save-excursion
          (re-search-backward (format "\[^\\\]%s" original))
          (forward-char)
          (delete-char 1)
          (insert replacement)
          (forward-char)
          (re-search-forward (format "\[^\\\]%s" original))
          (delete-char -1)
          (insert replacement))))))
;; " hej ' hej ' "


;; -*- emacs-lisp -*-
;; License: Gnu Public License
;;
;; Additional functionality that makes flymake error messages appear
;; in the minibuffer when point is on a line containing a flymake
;; error. This saves having to mouse over the error, which is a
;; keyboard user's annoyance

;;flymake-ler(file line type text &optional full-file)
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
	  (let ((err (car (second elem))))
	    (message "%s" (fly-pyflake-determine-message err)))))))

(defun fly-pyflake-determine-message (err)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
	((null (flymake-ler-file err))
	 ;; normal message do your thing
	 (flymake-ler-text err))
	(t ;; could not compile err
	 (format "compile error, problem on line %s" (flymake-ler-line err)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
  (set (make-local-variable 'post-command-hook)
       (cons 'show-fly-err-at-point post-command-hook)))

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (let (min max)
    (if (or (region-active-p) (eq last-command 'comment-or-uncomment-current-line-or-region))
        (comment-or-uncomment-region (region-beginning) (region-end))
      (progn
        (goto-char (line-beginning-position))
        (push-mark)
        (goto-char (line-end-position))
        (comment-or-uncomment-region (region-beginning) (region-end))))))

(defun coffee-to-js ()
  "Convert a coffee script region to javascript"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "coffee -p -s -b" nil t))

(defun js-to-coffee ()
  "Convert a javascript region to coffee script"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "js2coffee" nil t))

(defun projectile-toggle-test-code ()
  "Jump between test and implementation"
    (interactive)
    (when (buffer-file-name)
      (let ((lib-matches
             (s-match
              "agency\\/lib\/\\(.+\\)\\/\\(.+\\)\\.coffee$"
              (buffer-file-name)))
            (spec-matches
             (s-match
              "agency\\/test\\/unit\/\\(.+\\)\\/\\(.+\\)\\.spec\\.coffee$"
              (buffer-file-name))))
        (cond (lib-matches
               (find-file
                (format
                 "/Users/bjorne/Code/agency/test/unit/%s/%s.spec.coffee"
                 (nth 1 lib-matches)
                 (nth 2 lib-matches))))
              (spec-matches
               (find-file
                (format
                 "/Users/bjorne/Code/agency/lib/%s/%s.coffee"
                 (nth 1 spec-matches)
                 (nth 2 spec-matches))))))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(provide 'bjorne-defuns)
