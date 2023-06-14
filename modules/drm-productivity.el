;;; drm-productivity.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Packages and configuration related to productivity (org, md etc..)

;;; code:

;; packages

(straight-use-package 'org-modern)
(straight-use-package 'markdown-mode)
(straight-use-package 'denote)
(straight-use-package 'consult-notes)

;; config

;;; org babel
(setq org-confirm-babel-evaluate nil)

;;; org modern
(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

(global-org-modern-mode)

;;Org agenda
(setq org-agenda-files '("~/Nextcloud/org/Todo.org"))

(setq org-image-actual-width nil)
(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
(setq org-html-validation-link nil)

(autoload 'markdown-mode "markdown-mode"
  "major mode for editing markdown files" t)
(autoload 'gfm-mode "markdown-mode"
  "major mode for editing github flavored markdown files" t)
(setq markdown-command "multimarkdown")

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\*.org\\'" . org-mode))

;; Denote configuration
;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/Nextcloud/Notes/"))
(setq denote-known-keywords
      '("emacs" "programming" "general"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type 'org)
(setq denote-prompts '(title keywords))

;; We allow multi-word keywords by default.  The author's personal
;; preference is for single-word keywords for a more rigid workflow.
(setq denote-allow-multi-word-keywords t)

(consult-notes-denote-mode t)

(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

(let ((map global-map))
  (define-key map (kbd "C-c n n") #'denote)
  (define-key map (kbd "C-c n N") #'denote-type)
  (define-key map (kbd "C-c n d") #'denote-date)
  (define-key map (kbd "C-c n r") #'denote-dired-rename-file))

(with-eval-after-load 'org-mode
  (let ((map org-mode-map))
    (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "C-c n I") #'denote-link-add-links)
    (define-key map (kbd "C-c n l") #'denote-link-find-file) ; "list" links
    (define-key map (kbd "C-c n b") #'denote-link-backlinks)))

(global-set-key (kbd "C-c a") #'org-agenda)

(provide 'drm-productivity)
;;; drm-productivity.el ends here
