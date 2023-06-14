;;; drm-haskell.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages and configurations for the Haskell programming language

;;; Code:

;; packages

(straight-use-package 'haskell-mode)

;; config

(add-to-list 'auto-mode-alist '("\\*.hs\\'" . go-mode))

(add-hook 'haskell-mode-hook #'eglot-ensure)

(provide 'drm-haskell)
;;; drm-haskell.el ends here
