;;; drm-editing.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages and configuraitons related to editing.

;;; Code:

;; packages


(show-paren-mode 1)
(electric-pair-mode 1)

(use-package evil-nerd-commenter
  :init (evilnc-default-hotkeys))

(use-package ws-butler
  :init
  (add-hook 'text-mode-hook 'ws-butler-mode)
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package smartparens)

(setq org-export-allow-bind-keywords t)

(provide 'drm-editing)
;;; drm-editing.el ends here
