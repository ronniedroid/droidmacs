(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; package management
(setq straight-use-package-by-default t
      package-enable-at-startup nil
      straight-fix-flycheck t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; changing the defaults
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode t)
(global-display-line-numbers-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq backup-directory-alist '(("." . "~/.cache/emacssaves"))
      inhibit-startup-message t
      create-lockfiles nil
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      tab-width 2)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; eye candy
(use-package modus-vivendi-theme
  :config
  (setq modus-themes-vivendi-color-overrides
	'((bg-main . "#191a1b")))
  (setq modus-themes-hl-line '(intense))
  (load-theme 'modus-vivendi t))

(use-package which-key
  :config
  (which-key-mode))

(use-package all-the-icons
  :after dashboard)

(use-package echo-bell
  :custom
  (echo-bell-string "''")
  (echo-bell-background "#000000")
  :config
  (echo-bell-mode t))

(use-package page-break-lines
  :after dashboard
  :config
  (page-break-lines-mode))

;; mode line

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Emacs Dashboard

(use-package dashboard
  :custom
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-banner-logo-title "")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents  . 5)
  		     (projects . 5)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  :config
  (dashboard-setup-startup-hook))

;; completion

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; window management

(use-package shackle
  :custom
  (shackle-lighter "")
  (shackle-select-reused-windows nil) ; default nil
  (shackle-default-alignment 'below) ; default below
  (shackle-default-size 0.4) ; default 0.5
  (shackle-rules
   ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
   '((compilation-mode              :select nil                                               )
     ("*eshell*"                    :select t                          :align below :size 0.4 :popup t)
     ("*Shell Command Output*"      :select nil                                               )
     ("*Help*"                      :select t                          :align right :size 0.4 :popup t)
     ("*Completions*"                                                  :size 0.3  :align t    )
     ("*Messages*"                  :select nil :inhibit-window-quit t :other t               )
     ("*info*"                      :select t   :inhibit-window-quit t                         :same t)
     ))
  :init
  (shackle-mode 1))

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
	 ("M-`"   . popper-cycle)
	 ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Warnings\\*"
     "\\*eshell\\*"
     help-mode
     compilation-mode))
  (popper-display-control nil)
  :config
  (popper-mode +1))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :straight t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
	    (lambda ()
	      (unless (file-remote-p default-directory)
		(auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  :custom
  (dired-sidebar-subtree-line-prefix "__")
  (dired-sidebar-theme 'vscode)
  (dired-sidebar-use-term-integration t)
  (dired-sidebar-use-custom-font t))


;; org-mode and markdown
(use-package org
  :straight nil
  :mode ("\\*.org\\'")
  :hook
  (org-mode . org-indent-mode))

(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package ox-latex
  :straight nil
  :config
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x"))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown"))

;; version control

(use-package magit
  :bind
  ("C-x g" . magit-status))

;; coding

(use-package company
  :diminish
  :hook
  (after-init . global-company-mode)
  :custom
  (company-idle-delay 0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-web
  :after web-mode)

(use-package format-all
  :custom
  (format-all-show-errors 'never)
  (format-all-formatters '(
			   ("python" "black")
			   ("javascript" "prettier")
			   ("vuejs" "prettier")
			   ))
  :hook
  (format-all-mode . format-all-ensure-formatter)
  ((prog-mode before-save) . format-all-mode))

;; web-stuff
(use-package web-mode
  :mode
  ("\\.css?\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  :config
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))

(define-derived-mode my-vue-mode web-mode "vueMode"
  "a major mode derived from web-mode for editing vue files with eglot")

(use-package my-vue-mode
  :straight nil
  :mode
  ("\\.vue\\'" . my-vue-mode))

(use-package emmet-mode
  :custom
  (emmet-self-closing-tag-style " /")
  :hook
  ((web-mode my-vue-mode rjsx-mode) . emmet-mode))

;; javascript

(use-package rjsx-mode
  :mode
  ("\\.js\\'" . rjsx-mode)
  ("\\.jsx\\'" . rjsx-mode))

;; rust

(use-package rust-mode
  :mode ("\\*.rs\\'")
  :custom
  (rust-format-on-save t)
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil))))

;; Haskell

(use-package haskell-mode
  :mode ("\\*.hs\\'")
  :custom
  (haskell-stylish-on-save t))
;; lsp
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(my-vue-mode "vls"))
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  :hook
  ((my-vue-mode rjsx-mode python-mode rust-mode haskell-mode) . eglot-ensure))

(use-package eldoc
  :straight nil
  :hook
  (eglot-connect . eldoc-mode))

(use-package eldoc-box
  :commands (eldoc-box-hover-at-point-mode)
  :hook
  (eldoc-mode . eldoc-box-hover-at-point-mode))

;;; misc

(use-package kbd-mode
  :straight nil
  :load-path "~/.config/emacs/elisp/")

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
