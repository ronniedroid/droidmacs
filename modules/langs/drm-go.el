;;; drm-go.el -*- lexical-binding: t; -*-

(straight-use-package 'go-mode)

(add-to-list 'auto-mode-alist '("\\*.go\\'" . go-mode))

(add-hook 'go-mode-hook #'eglot-ensure)

(provide 'drm-go)
