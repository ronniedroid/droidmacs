;;; drm-programming.el -*- lexical-binding: t; -*-

;; install packages
(straight-use-package 'format-all)
(straight-use-package 'magit)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'xref)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'eglot)
(straight-use-package 'eldoc)
(straight-use-package 'restclient)
(straight-use-package 'ob-restclient)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)
   (emacs-lisp . t)))

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
			      ("vue" "prettier")
                              ("Astro" "astro-ls")
                              ("Clojure" (zprint "{:style :community :width 50}"))
			      ))
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
(add-hook 'prog-mode-hook 'format-all-mode)
(add-hook 'before-save-hook 'format-all-buffer)

(require 'eglot)
;; configure languages and their servers
(add-to-list 'eglot-server-programs '(drm-astro-mode . ("astro-ls" "--stdio")))
(add-to-list 'eglot-server-programs '(drm-vue-mode . ("vls" "--stdio")))
(add-to-list 'eglot-server-programs '(drm-html-mode . ("vscode-html-language-server" "--stdio")))
(add-to-list 'eglot-server-programs '(drm-css-mode . ("vscode-css-language-server" "--stdio")))
;; do not load language server capabilities that do not work in egot
(add-to-list 'eglot-ignored-server-capabilites :hoverProvider)

;; start eldoc when eglot is started
(add-hook 'eglot-managed-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(provide 'drm-programming)
