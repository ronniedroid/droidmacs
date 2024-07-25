;;; drm-programming.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages and configurations related to programming modes

;;; Code:

(use-package rainbow-delimiters
  :hook (prog-mode))

(use-package xref)

;; use system PATH for emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; set a keybindings for magit
(use-package magit
  :bind
  (("C-x g" . magit-status)))

;; setup formatting for programming languages
(use-package format-all
  :custom
  (format-all-show-errors 'never)
  (format-all-formatters '(
                           ("go" gpfmt)
                           ("Clojure" (zprint "{:style :community :width 50}"))
			   ))
  :hook
  (format-all-mode . format-all-ensure-formatter)
  (prog-mode . format-all-mode)
  (before-save . format-all-buffer))

;; Javascript
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . js2-mode)))

;; Golang
(use-package go-mode
  :config
  (add-to-list 'auto-mode-alist '("\\*.go\\'" . go-mode))
  :hook
  (go-mode . eglot-ensure))

;; Clojure

(use-package clojure-mode-extra-font-locking)

;; clojure mode configuration
(use-package clojure-mode
  :hook
  (clojure-mode . subword-mode))

(use-package flymake-kondor
  :hook
  (clojure-mode . flymake-kondor-setup))

(use-package cider
  :custom
  (cider-repl-wrap-history t)
  (cider-repl-history-file "~/.config/emacs/cider-history")
  (cider-save-file-on-load t)
  (cider-font-lock-dynamically '(macro core function var))
  (cider-repl-display-help-banner nil)
  :bind ("C-c C-j" . cider-jack-in)
  :hook
  (cider-repl-mode-hook . subword-mode)
  (cider-repl-mode-hook . rainbow-delimiters-mode))

;; Eglot

(use-package eglot
  :ensure nil
  :defer t
  :custom
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil)
  :config
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  :hook
  (clojure-mode . eglot-ensure)
  (js2-mode . eglot-ensure)
  (go-mode . eglot-ensure))


;; start eldoc when eglot is started
(use-package eldoc
  :ensure nil
  :hook
  (eglot-managed-mode . eldoc-mode)
  (emacs-lisp-mode . eldoc-mode))

(provide 'drm-programming)
;;; drm-programming.el ends here
