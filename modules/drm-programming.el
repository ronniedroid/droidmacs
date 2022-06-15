
;;; drm-programming.el -*- lexical-binding: t; -*-

;; install packages
(straight-use-package 'format-all)
(straight-use-package 'magit)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'xref)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'lsp-mode)

;; require lang specific configs
(require 'drm-web)
(require 'drm-rust)
(require 'drm-clojure)

;; use system PATH for emacs
(exec-path-from-shell-initialize)

;; set a keybindings for magit
(global-set-key ( kbd "C-x g") 'magit-status)

;; setup formatting for programming languages
(setq format-all-show-errors 'never)
(setq format-all-formatters '(
			      ("Python" "black")
			      ("JavaScript" "prettier")
			      ("vue" "prettier")
			      ("Svelte" "prettier")
                              ("Clojure" (zprint "{:style :community :width 45}"))
			      ))
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
(add-hook 'prog-mode-hook 'format-all-mode)
(add-hook 'before-save-hook 'format-all-mode)

(require 'lsp-mode)
(setq lsp-enable-snippet nil)

(provide 'drm-programming)
