;;; drm-editing.el -*- lexical-binding: t; -*-

(show-paren-mode 1)
(electric-pair-mode 1)

(straight-use-package 'ws-butler)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'avy)
(straight-use-package 'smartparens)
(straight-use-package 'composable)

;; Set up ws-butler for trimming whitespace and line endings
(add-hook 'text-mode-hook 'ws-butler-mode)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Set a global binding for better line commenting/uncommenting
(global-set-key (kbd "C-;") 'evilnc-comment-or-uncomment-lines)

;; avy bindings
(global-set-key (kbd "C-s s") 'avy-goto-char-timer)

(setq org-export-allow-bind-keywords t)

;; Composable configuration
(require 'composable)
(setq composable-repeat nil)
(setq composable-mode-line-color nil)
(setq composable-which-keys nil)
(global-set-key [remap kill-region] 'composable-kill-region)
(global-set-key [remap kill-ring-save] 'composable-kill-ring-save)
(put 'upcase-region 'disabled nil) ; enable uppercasing
(put 'downcase-region 'disabled nil) ; enable downcasing
(global-set-key [remap upcase-region] 'composable-upcase-region)
(global-set-key [remap downcase-region] 'composable-downcase-region)

(provide 'drm-editing)
