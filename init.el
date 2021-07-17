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


;; eye candy
(use-package modus-vivendi-theme
  :config
  (load-theme 'modus-vivendi t))

(use-package which-key
  :config
  (which-key-mode))

(use-package all-the-icons)

(use-package echo-bell
  :config
  (setq echo-bell-string "''"
	echo-bell-background "#000000")
  :init
  (echo-bell-mode t))

;; completion

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; usefull packages

(use-package popper
  :bind (("M-p"   . popper-toggle-latest)
         ("M-n"   . popper-cycle)
         ("M-`" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
	  "\\*Warnings\\*"
          help-mode
          compilation-mode))
  (popper-mode +1))

;; org-mode
(use-package org-bullets
  :hook
  (org-mode-hook . (lambda () (org-bullets-mode 1))))

;; version control

(use-package magit
  :bind
  ("C-x g" . magit-status))

;; coding

(use-package company
  :diminish
  :hook
  (after-init . global-company-mode))

(use-package company-box
  :config
  (setq company-idle-delay 0)
  :hook (company-mode . company-box-mode))

(use-package company-web)

(use-package format-all
  :config
  (setq format-all-show-errors 'never
	format-all-formatters '(
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
  :config
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-enable-current-column-highlight t
	web-mode-enable-current-element-highlight t)
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))

(define-derived-mode my-vue-mode web-mode "vueMode"
  "a major mode derived from web-mode for editing vue files with eglot")

(use-package my-vue-mode
  :straight nil
  :mode
  ("\\.vue\\'" . my-vue-mode))

(use-package emmet-mode
  :config
  (setq emmet-self-closing-tag-style " /")
  :hook
  ((web-mode my-vue-mode rjsx-mode) . emmet-mode))

;; javascript

(use-package rjsx-mode
  :mode
  ("\\.js\\'" . rjsx-mode)
  ("\\.jsx\\'" . rjsx-mode))

;; rust

(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil))))

;; lsp
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(my-vue-mode "vls"))
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  :hook
  ((my-vue-mode rjsx-mode python-mode) . eglot-ensure))

(use-package eldoc-box
  :after eldoc
  :config
  (eldoc-box-hover-mode))

(defconst emacs-start-time (current-time))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.2fs) [after-init]"
                        ,load-file-name elapsed))) t)

(provide 'init)
;;; init ends here
