;;; drm-editing.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Setup emacs's built in project package

;;; Code:

(use-package project
  :ensure nil
  :custom
  (project-switch-commands
      (append '((magit-status "Magit status"))
              project-switch-commands))
  :bind (("C-x p b" . consult-project-buffer)
         :map project-prefix-map
         ("v" . magit-status)))

(provide 'drm-project)
