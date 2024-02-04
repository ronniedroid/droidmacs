;;; drm-editing.el -*- lexical-binding: t; -*-

(straight-use-package 'project)

(require 'project)

(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "v") 'magit-status))

(global-set-key (kbd "C-x p b") 'consult-project-buffer)

(setq project-switch-commands
      (append '((magit-status "Magit status"))
              project-switch-commands))

(provide 'drm-project)
