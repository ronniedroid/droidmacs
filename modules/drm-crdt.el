;; drm-crdt.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; packages and configuration to make emacs' UI nicer and more functional

;;; Code:

;; packages

(straight-use-package 'crdt)


;; config

;; (setq crdt-tuntox-executable "~/.local/bin/tuntox")
(setq crdt-use-tuntox t)

(provide 'drm-crdt)
;;; drm-crdt.el ends here
