;;; drm-clojure.el -*- lexical-binding: t; -*-

(straight-use-package 'clojure-mode)
(straight-use-package 'clojure-mode-extra-font-locking)
(straight-use-package 'flymake-kondor)
(straight-use-package 'cider)

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

(add-hook 'clojure-mode-hook #'lsp)
;; (add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)

(provide 'drm-clojure)
