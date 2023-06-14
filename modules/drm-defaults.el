;;; drm-defaults.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Some sane defaults for emacs

;;; Code:

;; packages


(straight-use-package 'no-littering)

;; clean up after emacs
(require 'no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Do not saves duplicates in kill-ring
(setq kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-conservatively 101)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-S-s"))


;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(provide 'drm-defaults)
;;; drm-defaults.el ends here
