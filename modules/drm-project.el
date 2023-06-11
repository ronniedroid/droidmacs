;;; drm-editing.el -*- lexical-binding: t; -*-

(straight-use-package 'project)
;; (straight-use-package 'project-tab-groups)

(require 'project)
;; (require 'project-tab-groups)
;; (project-tab-groups-mode 1)

(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "g") 'magit-status)
  (define-key project-prefix-map (kbd "G") 'project-find-regexp))

(global-set-key (kbd "C-x p b") 'consult-project-buffer)

(setq project-switch-commands
      (append '((magit-status "Magit status"))
              project-switch-commands))

(provide 'drm-project)
