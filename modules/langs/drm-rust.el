;;; drm-rust.el -*- lexical-binding: t; -*-

(straight-use-package 'rust-mode)

;; rust mode configuration
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-to-list 'auto-mode-alist '("\\*.rs\\'" . rust-mode))

(add-hook 'rust-mode-hook #'lsp)

(provide 'drm-rust)
