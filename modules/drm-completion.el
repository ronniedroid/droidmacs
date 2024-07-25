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

(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

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
          ("C-r" . 'consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; Set up Orderless for better fuzzy matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic-save-buffer))
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
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  ;;:bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  )

(use-package kind-icon
  :custom
  (kind-icon-default-face 'corfu-default))

(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(provide 'drm-completion)
;;; drm-completion.el ends here
