;;; drm-evil.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages for Evil mode, for those who prefer `Vim' keybindings.
;; credit to crafted-emacs

;;; Code:

;; packages

(straight-use-package 'evil)
(straight-use-package 'evil-collection)

;; config

(setq evil-undo-system 'undo-redo)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-i-jump nil)
(setq evil-respect-visual-line-mode t)
(setq evil-want-C-h-delete t)

(evil-mode 1)

;; Make evil search more like vim
(evil-select-search-module 'evil-search-module 'evil-search)

(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "M-p") 'consult-yank-from-kill-ring)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Modes that should start in Emacs state
;; (dolist (mode '(custom-mode
;;                 eshell-mode
;;                 term-mode))
;;   (add-to-list 'evil-emacs-state-modes mode))

;; turn on evil collection
(evil-collection-init)

(provide 'drm-evil)
;;; drm-evil.el ends here
