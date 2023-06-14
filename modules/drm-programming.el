;;; drm-programming.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages and configurations related to programming modes

;;; Code:

;; packages

(straight-use-package 'format-all)
(straight-use-package 'magit)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'xref)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'eglot)
(straight-use-package 'eldoc)
(straight-use-package 'restclient)
(straight-use-package 'ob-restclient)

;; config

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)
   (emacs-lisp . t)))

;; require lang specific configs
(require 'drm-web)
(require 'drm-rust)
(require 'drm-clojure)
(require 'drm-go)
(require 'drm-haskell)

;; use system PATH for emacs
(exec-path-from-shell-initialize)

;; set a keybindings for magit
(global-set-key ( kbd "C-x g") 'magit-status)

;; setup formatting for programming languages
(setq format-all-show-errors 'never)
(setq format-all-formatters '(
			      ("Python" black)
			      ("vue" prettier)
                              ("Astro" astro-ls)
                              ("go" gpfmt)
                              ("Clojure" (zprint "{:style :community :width 50}"))
                              ("PHP" prettier)
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
(add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))
;; do not load language server capabilities that do not work in egot
(add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
;;config
(setq-default eglot-workspace-configuration
              '((haskell
                 (plugin
                  (stan
                   (globalOn . :json-false))))))
(setq eglot-autoshutdown t)  ;; shutdown language server after closing last file
(setq eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation

;; start eldoc when eglot is started
(add-hook 'eglot-managed-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(provide 'drm-programming)
;;; drm-programming.el ends here
