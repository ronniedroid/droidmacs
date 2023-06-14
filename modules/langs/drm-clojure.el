;;; drm-clojure.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages and configurations for the Clojure programming language

;;; Code:

;; packages

(straight-use-package 'clojure-mode)
(straight-use-package 'clojure-mode-extra-font-locking)
(straight-use-package 'flymake-kondor)
(straight-use-package 'cider)

;; config

(require 'clojure-mode-extra-font-locking)

;; clojure mode configuration
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'flymake-kondor-setup)

(global-set-key ( kbd "C-c C-j") 'cider-jack-in)

;; cider repl configuration
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(setq cider-repl-wrap-history t)
(setq cider-repl-history-file "~/.config/emacs/cider-history")
(setq cider-save-file-on-load t)
(setq cider-font-lock-dynamically '(macro core function var))
(setq cider-repl-display-help-banner nil)

(require 'cider)

(add-hook 'clojure-mode-hook #'eglot-ensure)

(provide 'drm-clojure)
;;; drm-clojure.el ends here
