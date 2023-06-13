;;; drm-editing.el -*- lexical-binding: t; -*-

(show-paren-mode 1)
(electric-pair-mode 1)

(straight-use-package 'ws-butler)
(straight-use-package 'smartparens)

;; Set up ws-butler for trimming whitespace and line endings
(add-hook 'text-mode-hook 'ws-butler-mode)
(add-hook 'prog-mode-hook 'ws-butler-mode)

(setq org-export-allow-bind-keywords t)

(provide 'drm-editing)
