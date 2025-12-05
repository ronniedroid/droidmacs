;;; drm-productivity.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages and configuration related to productivity (org, md etc..)

;;; code:

;;Org agenda
(use-package org
  :ensure nil
  :custom
  (org-agenda-files '("~/Nextcloud/org/Todo.org"))
  (org-image-actual-width nil)
  (org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (org-html-validation-link nil)
  (org-confirm-babel-evaluate nil)
  :bind ("C-c a" . org-agenda))

;;; Markdown mode
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\*.org\\'" . org-mode)
         )
  :init (setq markdown-command "multimarkdown"))

(provide 'drm-productivity)
;;; drm-productivity.el ends here
