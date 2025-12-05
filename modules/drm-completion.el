;;; drm-completion.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Completion packages and configurations.

;;; Code:

;; vertico mode settings
(use-package vertico
  :custom
  (vertico-cycle nil)
  :init
  (vertico-mode))

;; history in the completion
(use-package savehist
  :init
  (savehist-mode))

(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Configure Marginalia
(use-package marginalia
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(global-unset-key (kbd "C-s"))

;; Set some consult bindings
(use-package consult
  :bind ( ("C-s s" . isearch-forward)
          ("C-s b" . isearch-backward)
          ("C-s r s" . isearch-forward-regexp)
          ("C-s r b" . isearch-backward-regexp)
          ("C-s o" . occur)
          ("C-s l" . consult-line)
          ("C-s L" . consult-line-multi)
          ("C-s O" . consult-outline)
          ("C-s i" . consult-imenu)
          ("C-s I" . consult-imenu-multi)
          ("C-s g" . consult-goto-line)
          ("C-s k" . consult-kmacro)
          ("C-s f" . consult-flymake)
          ("C-s m" . consult-mark)
          :map minibuffer-local-map
          ("C-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; Set up Orderless for better fuzzy matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; corfu configuration
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  :init
  (global-corfu-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  )

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(provide 'drm-completion)
;;; drm-completion.el ends here
