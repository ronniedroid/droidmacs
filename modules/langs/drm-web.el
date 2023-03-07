;;; drm-web.el -*- lexical-binding: t; -*-

;; install packages
(straight-use-package 'emmet-mode)
(straight-use-package 'web-mode)
(straight-use-package 'js2-mode)
(straight-use-package 'php-mode)

;; web mode configuration
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-engines-alist '(("blade" . "\\.blade\\.")))

(define-derived-mode drm-vue-mode web-mode "Vue "
  "a major mode derived from web-mode for editing vue files with eglot")

(define-derived-mode drm-astro-mode web-mode "Astro"
  "a major mode derived from web-mode for editing vue files with eglot")

(define-derived-mode drm-css-mode web-mode "CSS "
  "a major mode derived from web-mode for editing CSS files with eglot")

(define-derived-mode drm-html-mode web-mode "HTML "
  "a major mode derived from web-mode for editing CSS files with eglot")

(define-derived-mode drm-blade-mode web-mode "Blade "
  "a major mode derived from web-mode for editing blade files with eglot")

(add-to-list 'auto-mode-alist '("\\.html\\'" . drm-html-mode))
(add-to-list 'auto-mode-alist '("\\.astro\\'" . drm-astro-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . drm-css-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . drm-vue-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . drm-blade-mode))

;; setup emmet
(setq emmet-self-closing-tag-style " /")
(add-hook 'drm-html-mode-hook 'emmet-mode)
(add-hook 'drm-vue-mode-hook 'emmet-mode)
(add-hook 'drm-css-mode-hook 'emmet-mode)
(add-hook 'drm-astro-mode-hook 'emmet-mode)

(add-hook 'drm-vue-mode-hook #'eglot-ensure)
(add-hook 'drm-html-mode-hook #'eglot-ensure)
(add-hook 'js2-mode-hook #'eglot-ensure)
(add-hook 'drm-css-mode-hook #'eglot-ensure)
(add-hook 'drm-astro-mode-hook 'eglot-ensure)
(add-hook 'php-mode-hook 'eglot-ensure)

(provide 'drm-web)
