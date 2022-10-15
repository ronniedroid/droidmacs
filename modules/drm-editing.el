;;; drm-editing.el -*- lexical-binding: t; -*-

(show-paren-mode 1)
(electric-pair-mode 1)

(straight-use-package 'ws-butler)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'avy)
(straight-use-package 'smartparens)

;; Set up ws-butler for trimming whitespace and line endings
(add-hook 'text-mode-hook 'ws-butler-mode)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Set a global binding for better line commenting/uncommenting
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)

;; avy bindings
(global-set-key (kbd "C-; ;") 'avy-goto-char-timer)
(global-set-key (kbd "C-; o") 'avy-goto-char)

(setq org-export-allow-bind-keywords t)

(provide 'drm-editing)
