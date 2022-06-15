;;; drm-web.el -*- lexical-binding: t; -*-

;; install packages
(straight-use-package 'emmet-mode)
(straight-use-package 'web-mode)
(straight-use-package 'rjsx-mode)
(straight-use-package
 '(lsp-volar :type git :host github :repo "jadestrong/lsp-volar"))

;; setup emmet
(setq emmet-self-closing-tag-style " /")
(add-hook 'web-mode 'emmet-mode)
(add-hook 'my-vue-mode 'emmet-mode)
(add-hook 'my-svelte-mode 'emmet-mode)
(add-hook 'rjsx-mode 'emmet-mode)

;; web mode configuration
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

(define-derived-mode my-vue-mode web-mode "vueMode"
  "a major mode derived from web-mode for editing vue files with eglot")

(setq auto-mode-alist
      (append '(("\\.html\\'" . web-mode)
                ("\\.css\\'" . web-mode)
                ("\\.js\\'" . rjsx-mode)
                ("\\.ts\\'" . rjsx-mode)
                ("\\.cjs\\'" . rjsx-mode)
                ("\\.jsx\\'" . rjsx-mode)
                ("\\.vue\\'" . my-vue-mode))
              auto-mode-alist))

;; make sure eglot works for the major modes of the needed languages
(add-hook 'my-vue-mode-hook 'eglot-ensure)
(add-hook 'rjsx-mode-hook 'eglot-ensure)

(provide 'drm-web)
