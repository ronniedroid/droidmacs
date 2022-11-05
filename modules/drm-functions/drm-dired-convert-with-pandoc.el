(defun drm/convert-with-pandoc (name &optional arg)
  "Convert current file to another format with pandoc"
  (interactive "FOutput file: \nP")
  (let ((string
         (or (dired-get-subdir)
	     (mapconcat #'identity
                        (if arg
			    (cond ((zerop (prefix-numeric-value arg))
				   (dired-get-marked-files))
				  ((consp arg)
				   (dired-get-marked-files t))
				  (t
				   (dired-get-marked-files
				    'no-dir (prefix-numeric-value arg))))
			  (dired-get-marked-files 'no-dir))
                        " "))))
    (unless (string= string "")
      (if (<= (string-to-number
	       (substring (dired-number-of-marked-files) 0 1))
	      (string-to-number "1"))
	  (cond ((message string name)
		 (shell-command (concat "pandoc " "'" string "'" " -o " name))
		 (dired-revert)))
	(message "more than one file marked")))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c c") 'drm/convert-with-pandoc)
  )

(provide 'drm-dired-convert-with-pandoc)
