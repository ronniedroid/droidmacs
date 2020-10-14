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

;; changing the defaults
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(global-display-line-numbers-mode)

;;;; Theming and UI

;; set fonts
(set-frame-font "CodeNewRoman Nerd Font Mono-12" t t)

(use-package modus-operandi-theme
  :ensure t)

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
  (turn-on-page-break-lines-mode))

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
    ;;(setq aw-scope frame)
    (setq aw-dispatch-always t)))

;; avy

(use-package avy
  :ensure t
  :bind
  ("C-'" . avy-goto-char-2)
  ("C-:" . avy-goto-line)
  ("C-]" . avy-goto-end-of-line))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(modus-vivendi-theme magit ace-window dashboard projectile ivy-rich counsel ivy page-break-lines doom-modeline modus-operandi-theme which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
