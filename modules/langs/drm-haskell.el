;;; drm-haskell.el -*- lexical-binding: t; -*-

(straight-use-package 'haskell-mode)

(add-to-list 'auto-mode-alist '("\\*.hs\\'" . go-mode))

(add-hook 'haskell-mode-hook #'eglot-ensure)

(provide 'drm-haskell)
