(defun xiki-complete ()
  (interactive)
  (let (indent) 
    (when (save-excursion
            (goto-char (line-end-position))
            (looking-back "^\\(.*\\)\$\\W+\\(.+\\)"))
      (setq indent (match-string-no-properties 1))
      (save-excursion
        (goto-char (line-beginning-position 2))
        (while (looking-at-p (concat indent "\>  "))
          (delete-region (line-beginning-position) (line-beginning-position 2))))
      (let* ((res (xiki-run-bash (match-string-no-properties 2))) (lines (split-string res "\n")))
        (when res
          (save-excursion
            (goto-char (line-end-position))
            (mapc
             (lambda (line)
               (newline)
               (insert indent ">  " line)) lines)))))))

(defun xiki-run-bash (command)
  (with-temp-buffer
    (condition-case err
        (progn
          (call-process (executable-find "bash") nil (current-buffer) nil "-c" command)
          (buffer-string))
      (error
       (message "Command not found: %s" (error-message-string err))
       nil))))

(xiki-run-bash "pwd 1")

(defun xiki-run-command (command)
  (let ((regex "\\(\\w+\\)\\(.*\\)") program args)
    (when (string-match regex command)
      (setq program (match-string-no-properties 1 command))
      (setq args (delete "" (split-string (match-string-no-properties 2 command) " ")))
      (print args)
      (with-temp-buffer
        (condition-case err
            (progn
              (apply 'call-process (append (list program nil (current-buffer) nil) args))
              (buffer-string))
          (error
           (message "Command not found: %s" (error-message-string err))
           nil))))))

;; (xiki-run-command "ls -l")
;; $ pwd
;; >  /Users/bjorne/Code/emacs
;; >  
;; $ echo test
;; >  test
;; >  


;; $ ls -al
