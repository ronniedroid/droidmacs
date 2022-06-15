;;; drm-editing.el -*- lexical-binding: t; -*-

(straight-use-package 'project)

(require 'project)

(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "g") 'magit-status)
  (define-key project-prefix-map (kbd "G") 'project-find-regexp))

(setq project-switch-commands
      (append '((magit-status "Magit status"))
              project-switch-commands))

;; (defun project-other-tab-command ()
;;   "Do something useful."
;;   (interactive)
;;   (let ((tab-name (car (last (project-current)))))
;;     (project--other-place-command '((display-buffer-in-new-tab)))
;;     (tab-bar-rename-tab tab-name)))


;; (split-string (car (last (project-current))) "/")

(provide 'drm-projects)
