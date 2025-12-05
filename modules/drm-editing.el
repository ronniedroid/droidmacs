;;; drm-editing.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages and configuraitons related to editing.

;;; Code:

;; packages

(show-paren-mode 1)
(electric-pair-mode 1)

(use-package evil-nerd-commenter
  :bind (("C-; C-;" . evilnc-comment-or-uncomment-lines)
         ("C-; C-t" . evilnc-comment-or-uncomment-to-the-line)
         ("C-; C-r" . comment-or-uncomment-region)
         ("C-; C-k" . evilnc-comment-and-kill-ring-save)
         ("C-; C-b" . evilnc-comment-box)
         ("C-; C-c" . evilnc-copy-and-comment-lines)))

(global-unset-key (kbd "C-c p"))

(use-package ws-butler
  :init
  (add-hook 'text-mode-hook 'ws-butler-mode)
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package smartparens)

(setq org-export-allow-bind-keywords t)

(provide 'drm-editing)
;;; drm-editing.el ends here
