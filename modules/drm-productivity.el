;;; drm-productivity.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages and configuration related to productivity (org, md etc..)

;;; code:

;;; org modern
(use-package org-modern
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  :init
  (global-org-modern-mode))

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

;; Denote configuration
(use-package denote
  :custom
  (denote-directory (expand-file-name "~/Nextcloud/Notes/"))
  (denote-save-buffers nil)
  (denote-known-keywords '("emacs" "programming" "general"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type 'markdown-toml)
  (denote-prompts '(title keywords))
  (denote-excluded-directories-regexp nil)
  (denote-excluded-keywords-regexp nil)
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (denote-date-prompt-use-org-read-date t)
  (denote-date-format nil) ; read doc string
  (denote-backlinks-show-context t)
  (denote-dired-directories
   (list denote-directory
         (expand-file-name "attachments" denote-directory)))
  :hook
  (text-mode . denote-fontify-links-mode-maybe)
  (dired-mode . denote-dired-mode-in-directories)
  (context-menu-functions . denote-context-menu)
  :bind
  (("C-c n n" . denote)
   ("C-c n c" . denote-region) ; "contents" mnemonic
   ("C-c n N" . denote-type)
   ("C-c n d" . denote-date)
   ("C-c n z" . denote-signature) ; "zettelkasten" mnemonic
   ("C-c n s" . denote-subdirectory)
   ("C-c n t" . denote-template)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n i" . denote-link) ; "insert" mnemonic
   ("C-c n I" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n f f" . denote-find-link)
   ("C-c n f b" . denote-find-backlink)
   :map dired-mode-map
   ("C-c C-d C-i" . denote-link-dired-marked-notes)
   ("C-c C-d C-r" . denote-dired-rename-files)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (denote-rename-buffer-mode 1))


(use-package consult-notes
  :after denote
  :init
  (consult-notes-denote-mode))

(provide 'drm-productivity)
;;; drm-productivity.el ends here
