(setq gc-cons-threshold (* 50 1000 1000))

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
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)
(global-display-line-numbers-mode)
(setq tab-always-indent 'complete)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq org-hide-emphasis-marker t)
(setq backup-directory-alist '(("." . "~/.cache/emacssaves"))
      inhibit-startup-message t
      create-lockfiles nil
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      tab-width 2)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-S-s"))

;; eye candy
(use-package modus-themes
  :config
  (setq modus-themes-hl-line '(intense))
  (load-theme 'modus-operandi t))

(use-package modus-themes-exporter
  :straight nil
  :load-path "~/.config/emacs/lib")

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
  :hook
  (dashboard-mode . (lambda () (setq global-hl-line-mode nil)))
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

(use-package consult
  :bind
  ("C-c b" . consult-bookmark)
  ("C-c k" . consult-kmacro)
  ("C-c f" . consult-flymake)
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ("M-g g" . consult-goto-line)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("C-s g" . consult-ripgrep)
  ("C-s l" . consult-line)
  ("C-s m" . consult-line-multi)
  ("C-s u" . consult-focus-lines)
  ("M-y" . consult-yank-pop)
  :init
  (setq register-preview-delay 0
	register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key 'any)
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-project-root-function
	(lambda ()
	  (when-let (project (project-current))
	    (car (project-roots project))))))

;; region completion

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary nil)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-echo-documentation nil) ;; Do not show documentation in the echo area
  :init
  (corfu-global-mode))

;; Dabbrev works with Corfu
(use-package dabbrev)

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
     ("*Org Export Dispatcher*"     :select t   :inhibit-window-quit t :align right :size 0.4 :popup t)
     ("*Completions*"                                                  :size 0.3  :align t    )
     ("*Messages*"                  :select nil :inhibit-window-quit t :other t               )
     ("*info*"                      :select t   :inhibit-window-quit t                         :same t)
     ))
  :init
  (shackle-mode 1))

(use-package popper
  :bind
  ("C-c p p" .  popper-toggle-latest)
  ("C-c p n" .   popper-cycle)
  ("C-c p t" . popper-toggle-type)
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
  :bind ("C-x g" .  magit-status))

;; coding

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
  ("\\.html\\'" . web-mode)
  ("\\.css\\'" . web-mode)
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

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
