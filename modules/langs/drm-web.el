;;; drm-web.el -*- lexical-binding: t; -*-

;; install packages
(straight-use-package 'emmet-mode)
(straight-use-package 'web-mode)
(straight-use-package 'rjsx-mode)

;; web mode configuration
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

(define-derived-mode my-vue-mode web-mode "Vue "
  "a major mode derived from web-mode for editing vue files with lsp")

(define-derived-mode my-css-mode web-mode "CSS "
  "a major mode derived from web-mode for editing svelte files with lsp")

(setq auto-mode-alist
      (append '(("\\.html\\'" . web-mode)
                ("\\.css\\'" . my-css-mode)
                ("\\.js\\'" . rjsx-mode)
                ("\\.ts\\'" . rjsx-mode)
                ("\\.cjs\\'" . rjsx-mode)
                ("\\.jsx\\'" . rjsx-mode)
                ("\\.vue\\'" . my-vue-mode))
              auto-mode-alist))

;; setup emmet
(setq emmet-self-closing-tag-style " /")
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'my-vue-mode-hook 'emmet-mode)
(add-hook 'my-svelte-mode-hook 'emmet-mode)
(add-hook 'my-css-mode-hook 'emmet-mode)
(add-hook 'rjsx-mode-hook 'emmet-mode)

(add-hook 'my-vue-mode-hook #'lsp)
(add-hook 'my-svelte-mode-hook #'lsp)
(add-hook 'rjsx-mode-hook #'lsp)
(add-hook 'my-css-mode-hook #'lsp)

(provide 'drm-web)
