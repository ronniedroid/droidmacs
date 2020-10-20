;; enabling melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; setting up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

;; changing the defaults
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode t)
(global-display-line-numbers-mode)
(setq backup-directory-alist '(("." . "~/.cache/emacssaves")))
(defalias 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode 1)
(setq create-lockfiles nil)

;;;; Theming and UI

;; set fonts
(setq default-frame-alist `(
			    (font . "CodeNewRoman Nerd Font Mono-12")
			    (fullscreen . maximized)
			    (undecorated . t)
			    (vertical-scroll-bars . nil)
			    (horizontal-scroll-bars . nil)
			    ))

(use-package modus-operandi-theme
  :disabled)

(use-package modus-vivendi-theme
  :ensure t
  :config
  (load-theme 'modus-vivendi t))

;; setting up which key
(use-package which-key
  :ensure t

  :config (which-key-mode))

;; doom mode-line
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

;; install all the icons fonts
(use-package all-the-icons
  :ensure t)

;; page-break-lines
(use-package page-break-lines
  :ensure t
  :config
  (page-break-lines-mode))

;; Emacs Dashboard

(use-package dashboard
  :ensure t
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title "")
  (setq dashboard-center-content nil)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 5)
			  (projects . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner "~/.emacs.d/emacs.png")
  (dashboard-setup-startup-hook))

;;;; packages

;; ivy, ivy-rich, counsel and swiper

(use-package ivy
  :ensure t
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :after (ivy)
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :ensure t
  :after (ivy councel)
  :config
  (ivy-rich-mode 1))

(use-package swiper
  :ensure t
  :after (ivy)
  :bind
  ("C-s" . swiper))

;; Projectile

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :config
  (counsel-projectile-mode))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

;; Ace Widnow

(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key (kbd "C-x o") 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (setq aw-background nil)
    (setq aw-scope 'frame)
    (setq aw-dispatch-always t)))

;; avy

(use-package avy
  :ensure t
  :bind
  ("C-' '" . avy-goto-char-2)
  ("C-' [" . avy-goto-line)
  ("C-' ]" . avy-goto-end-of-line))

(use-package expand-region
  :ensure t
  :bind
  ("C-; ;" . er/expand-region)
  ("C-; -" . er/contract-region)
  ("C-; i p" . er/mark-inside-pairs)
  ("C-; o p" . er/mark-outside-pairs)
  ("C-; i q" . er/mark-inside-quotes)
  ("C-; o q" . er/mark-outside-quotes)
  ("C-; c" . er/mark-comment)
  ("C-; u" . er/mark-url)
  ("C-; e" . er/mark-email)
  ;;("C-; s" . er/mark-sentence)
  ;;("C-; p" . er/mark-paragraph)
  ("C-; h a" . er/mark-html-attribute)
  ("C-; h i" . er/mark-inner-tag)
  ("C-; h o" . er/mark-outer-tag)
  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package centaur-tabs
  :ensure t
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-icons t)
  (centaur-tabs-plain-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "•")
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-mode t)
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  (vterm-mode . centaur-tabs-local-mode)
  :bind
  ("C-x t [" . centaur-tabs-backward)
  ("C-x t ]" . centaur-tabs-forward)
  ("C-x t p" . centaur-tabs-group-by-projectile-project)
  ("C-x t g" . centaur-tabs-group-buffer-groups))

(use-package company
  :ensure t
  :diminish
  :hook
  (after-init . global-company-mode))

(use-package company-box
  :ensure t
  :custom
  (company-idle-delay 0)
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; for javascript

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

(use-package prettier-js
  :ensure t
  :after (rjsx-mode)
  :hook (rjsx-mode . prettier-js-mode))

;; LSP MODE

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure t
  :hook ((rjsx-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands
  lsp)

;; optionally
(use-package lsp-ui
  :ensure t
  :commands
  lsp-ui-mode)
(use-package lsp-ivy
  :ensure t
  :commands
  lsp-ivy-workspace-symbol)
;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; yasnippits setup

(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs `("~/.emacs.d/mysnippets" "~/.emacs.d/snippets"))
  :init
  (yas-global-mode))

(use-package emmet-mode
  :ensure t
  :custom
  (emmet-expand-jsx-className? t)
  (emmet-self-closing-tag-style " /")
  :hook
  (sgml-mode . emmet-mode)
  (css-mode .  emmet-mode))

(use-package vterm
  :ensure-system-package (cmake libtool libvterm)
  :ensure t
  :custom
  (vterm-copy-exclude-prompt t)
  (vterm-always-compile-module t))

(use-package multi-vterm
  :ensure t
  :after (vterm)
  :bind
  ("C-c t o" . multi-vterm)
  ("C-c t n" . multi-vterm-next)
  ("C-c t p" . multi-vterm-prev)
  ("C-c t t" . multi-vterm-dedicated-toggle)
  ("C-c t p" . multi-vterm-project))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vterm use-package-ensure-system-package emmet-mode yassnippet-snippets yasnippet-snippets react-snippets yasnippet lsp-ivy lsp-ui lsp-mode counsel-projectile prettier-js rjsx-mode flycheck company-box company centaur-tabs which-key use-package projectile org-bullets modus-vivendi-theme modus-operandi-theme magit ivy-rich expand-region doom-modeline dashboard counsel ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
