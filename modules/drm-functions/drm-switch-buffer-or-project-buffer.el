(defun drm-switch-buffer-or-project-buffer ()
  "weather to switch all buffer or current project buffers"
  (interactive)
  (if (eq (project-current) nil)
      (consult-buffer)
    (call-interactively #'project-switch-to-buffer)))

(provide 'drm-switch-buffer-or-project-buffer)
