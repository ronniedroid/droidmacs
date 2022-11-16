(defun drm-tab-bar-tab-name-current-or-project ()
  "Generate tab name from the buffer of the selected window."
  (if (project-current)
      (thread-first (project-current)
                    last
                    car
                    (split-string "/")
                    butlast
                    last
                    car)
    (buffer-name (window-buffer (minibuffer-selected-window)))))

(provide 'drm-tab-bar-tab-name-current-or-project)
