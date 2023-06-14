;;; drm-go.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages and configurations for the Go programming language

;;; Code:

;; packages

(straight-use-package 'go-mode)

;; config

(add-to-list 'auto-mode-alist '("\\*.go\\'" . go-mode))

(add-hook 'go-mode-hook #'eglot-ensure)

(provide 'drm-go)
;;; drm-go.el ends here
