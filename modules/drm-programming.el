;;; drm-programming.el -*- lexical-binding: t; -*-

;; install packages
(straight-use-package 'format-all)
(straight-use-package 'magit)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'emmet-mode)
(straight-use-package 'web-mode)
(straight-use-package 'rjsx-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'eglot)
(straight-use-package 'eldoc)
(straight-use-package 'eldoc-box)
(straight-use-package 'xref)

;; use system PATH for emacs
(exec-path-from-shell-initialize)

;; set a keybindings for magit
(global-set-key ( kbd "C-x g") 'magit-status)

;; setup emmet
(setq emmet-self-closing-tag-style " /")
(add-hook 'web-mode 'emmet-mode)
(add-hook 'my-vue-mode 'emmet-mode)
(add-hook 'my-svelte-mode 'emmet-mode)
(add-hook 'rjsx-mode 'emmet-mode)

;; setup formatting for programming languages
(setq format-all-show-errors 'never)
(setq format-all-formatters '(
			      ("python" "black")
			      ("javascript" "prettier")
			      ("vuejs" "prettier")
			      ("Svelte" "prettier")
			      ))
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
(add-hook 'prog-mode-hook 'format-all-mode)
(add-hook 'before-save-hook 'format-all-mode)

;; programming languages and related packages
;; web mode configuration
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

(define-derived-mode my-vue-mode web-mode "vueMode"
  "a major mode derived from web-mode for editing vue files with eglot")

(define-derived-mode my-svelte-mode web-mode "svelteMode"
  "a major mode derived from web-mode for editing svelte files with eglot")


(setq auto-mode-alist
      (append '(("\\.html\\'" . web-mode)
                ("\\.css\\'" . web-mode)
                ("\\.js\\'" . rjsx-mode)
                ("\\.ts\\'" . rjsx-mode)
                ("\\.cjs\\'" . rjsx-mode)
                ("\\.jsx\\'" . rjsx-mode)
                ("\\.vue\\'" . my-vue-mode)
                ("\\.svelte\\'" . my-svelte-mode))
              auto-mode-alist))

;; rust mode configuration
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-to-list 'auto-mode-alist '("\\*.rs\\'" . rust-mode))

;; eglot configuration
(require 'eglot)
;; configure languages and their servers
(setq eglot-server-programs
      (append '((my-vue-mode . "vls")
                (my-svelte-mode . "svelte-language-server"))
              eglot-server-programs))
;; do not load language server capabilities that do not work in egot
(add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
;; make sure eglot works for the major modes of the needed languages
(add-hook 'my-vue-mode-hook 'eglot-ensure)
(add-hook 'my-svelte-mode-hook 'eglot-ensure)
(add-hook 'rjsx-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)

;; start eldoc when eglot is started
(add-hook 'eglot-connect 'eldoc-mode)
;; make eldoc documention a little bit nicer by showing it in a popup in the top right of the window
;; you can also use 'eldoc-box-hover-at-point-mode to show the popup right next to the cursor.
(autoload 'eldoc-box-hover-mode "eldoc-box")
(add-hook 'eldoc-mode 'eldoc-box-hover-mode)

(provide 'drm-programming)
