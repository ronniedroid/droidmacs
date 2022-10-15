;;; drm-editing.el -*- lexical-binding: t; -*-

(straight-use-package 'project)

(require 'project)
(tab-bar-mode 1)

(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "g") 'magit-status)
  (define-key project-prefix-map (kbd "G") 'project-find-regexp))

(setq project-switch-commands
      (append '((magit-status "Magit status"))
              project-switch-commands))

(defun tab-bar-tab-name-current-or-project ()
  "Generate tab name from the buffer of the selected window."
  (if (project-current)
      (thread-first (project-current)
                    last
                    car
                    (split-string "/")
                    butlast
                    last
                    car
                    ((lambda (s) (concat " " s))))
    (buffer-name (window-buffer (minibuffer-selected-window)))))

(setq tab-bar-tab-name-function #'tab-bar-tab-name-current-or-project)

(provide 'drm-projects)
