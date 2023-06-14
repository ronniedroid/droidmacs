;;; drm-rust.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages and configurations for the Rust programming language

;;; Code:

;; packages

(straight-use-package 'rust-mode)

;; config

(setq rust-format-on-save t)
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-to-list 'auto-mode-alist '("\\*.rs\\'" . rust-mode))

(add-hook 'rust-mode-hook #'eglot-ensure)

(provide 'drm-rust)
;;; drm-rust.el ends here
