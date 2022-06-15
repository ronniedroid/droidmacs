;;; drm-programming.el -*- lexical-binding: t; -*-

;; install packages
(straight-use-package 'format-all)
(straight-use-package 'magit)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'eglot)
(straight-use-package 'eldoc)
(straight-use-package 'eldoc-box)
(straight-use-package 'xref)
(straight-use-package 'rainbow-delimiters)

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
                              ("Clojure" (zprint "{:style :community :width 50}"))
			      ))
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
(add-hook 'prog-mode-hook 'format-all-mode)
(add-hook 'before-save-hook 'format-all-mode)

;; eglot configuration
(require 'eglot)
;; configure languages and their servers
(setq eglot-server-programs
      (append '(
                (my-vue-mode . ("vls"))
                (my-svelte-mode . "svelte-language-server"))
              eglot-server-programs))
;; do not load language server capabilities that do not work in egot
(add-to-list 'eglot-ignored-server-capabilites :hoverProvider)

;; start eldoc when eglot is started
(add-hook 'eglot-connect 'eldoc-mode)
;; make eldoc documention a little bit nicer by showing it in a popup in the top right of the window
;; you can also use 'eldoc-box-hover-at-point-mode to show the popup right next to the cursor.
(autoload 'eldoc-box-hover-mode "eldoc-box")
(add-hook 'eldoc-mode 'eldoc-box-hover-mode)

(provide 'drm-programming)
