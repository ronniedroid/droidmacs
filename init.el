;; package management
(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil)
(setq straight-fix-flycheck t)

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
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode t)
(global-display-line-numbers-mode)
(setq backup-directory-alist '(("." . "~/.cache/emacssaves")))
(defalias 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode 1)
(setq create-lockfiles nil)
(setq tab-width 2)
(show-paren-mode 1)


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
  (setq echo-bell-string "''")
  (setq echo-bell-background "#000000")
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
  ;;  :bind (("M-A" . marginalia-cycle)
  ;;         :map minibuffer-local-map
  ;;         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; usefull packages

(use-package popper
  :ensure t ; or :straight t
  :bind (("M-p"   . popper-toggle-latest)
         ("M-n"   . popper-cycle)
         ("M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
	  "\\*Warnings\\*"
          help-mode
          compilation-mode))
  (popper-mode +1))

;; org-mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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
  :custom
  (company-idle-delay 0)
  :hook (company-mode . company-box-mode))

(use-package company-web)

(use-package yasnippet
  :config
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets)

(use-package format-all
  :custom
  (setq format-all-show-errors 'never)
  (setq format-all-formatters '(
				("python" "black")
				("javascript" "prettier")
				("vuejs" "prettier")
				))
  :hook
  (format-all-mode . format-all-ensure-formatter)
  (prog-mode . format-all-mode)
  (before-save . format-all-mode))

;; web-stuff
(use-package web-mode
  :mode
  ("\\.css?\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
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
  (web-mode . emmet-mode)
  (my-vue-mode . emmet-mode)
  (rjsx-mode . emmet-mode))

;; javascript

(use-package rjsx-mode
  :ensure t
  :mode
  ("\\.js\\'" . rjsx-mode)
  ("\\.jsx\\'" . rjsx-mode))

;; rust

(use-package rust-mode
  :custom
  (setq rust-format-on-save t)
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil))))

;; lsp
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(my-vue-mode "vls"))
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-hook 'my-vue-mode-hook #'eglot-ensure)
  (add-hook 'rjsx-mode-hook #'eglot-ensure)
  (add-hook 'python-mode #'eglot-ensure))

(provide 'init)
;;; init ends here
