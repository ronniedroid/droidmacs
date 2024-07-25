;;; drm-windows.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Window rules and configurations.

;;; Code:

;; shackle configurations
(use-package shackle
  :custom
  (shackle-lighter "")
  (shackle-select-reused-windows nil) ; default nil
  (shackle-default-alignment 'below) ; default below
  (shackle-default-size 0.4) ; default 0.5
  (shackle-rules
   ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
   '((compilation-mode              :select nil)
     ("\\*\\(?:[[:word:]]*-\\)*eshell\\*"  :select t                :align 'below :size 0.4 :popup t :regexp t)
     ("*Shell Command Output*"      :select nil)
     (helpful-mode                  :select t                            :align 'right :size 0.4 :popup t)
     ("*Org Export Dispatcher*"     :select t   :inhibit-window-quit t   :align 'right :size 0.4 :popup t)
     ("*Completions*"                                                    :align t     :size 0.3)
     ("*Messages*"                  :select nil :inhibit-window-quit nil :align 'right :size 0.3 :popup t)
     ("*info*"                      :select t   :inhibit-window-quit t                        :same t)
     ))
  :init
  (shackle-mode 1))

(global-unset-key (kbd "C-c p"))

(use-package popper
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Warnings\\*"
     "-eshell\\*$"
     eshell-mode
     help-mode
     Helpful-mode
     compilation-mode))
  (popper-display-control nil)
  :bind (("C-c p n" . popper-cycle)
         ("C-c p t" . popper-toggle-type)
         ("C-c p p" . popper-toggle))
  :init
  (popper-mode +1))

(provide 'drm-windows)
