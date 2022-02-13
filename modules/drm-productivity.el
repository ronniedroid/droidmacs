;;; drm-productivity.el -*- lexical-binding: t; -*-

(straight-use-package 'org-bullets)
(straight-use-package 'markdown-mode)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))

(autoload 'markdown-mode "markdown-mode"
  "major mode for editing markdown files" t)
(autoload 'gfm-mode "markdown-mode"
  "major mode for editing github flavored markdown files" t)
(setq markdown-command "multimarkdown")

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\*.org\\'" . org-mode))

(provide 'drm-productivity)
