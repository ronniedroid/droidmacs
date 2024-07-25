;;; drm-wk.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; View which key you are pressing and what keys are available to you.
;; credit to distro-tube

;;; Code:

(use-package which-key
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  (which-key-side-window-max-height 0.25)
  (which-key-idle-delay 0.8)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit t)
  (which-key-separator " → " )
  :init
  (which-key-mode))

(provide 'drm-wk)
;;; drm-wk.el ends here
