;;; drm-completion.el -*- lexical-binding: t; -*-

(straight-use-package 'vertico)
(straight-use-package 'consult)
(straight-use-package 'orderless)
(straight-use-package 'marginalia)
(straight-use-package 'savehist)
(straight-use-package 'corfu)
(straight-use-package 'cape)
(straight-use-package 'dabbrev)
(straight-use-package 'kind-icon)

;; vertico mode settings

(require 'vertico)
(setq vertico-cycle nil)
(vertico-mode 1)

;; history in the completion
(savehist-mode)

;; Configure Marginalia
(require 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

;; Set some consult bindings
(global-set-key (kbd "C-s k") 'consult-kmacro)
(global-set-key (kbd "C-s f") 'consult-flymake)
(global-set-key (kbd "C-s g") 'consult-goto-line)
(global-set-key (kbd "C-s o") 'consult-outline)
(global-set-key (kbd "C-s m") 'consult-mark)
(global-set-key (kbd "C-s l") 'consult-line)
(global-set-key (kbd "C-s L") 'consult-line-multi)
(global-set-key (kbd "M-y") 'consult-yank-from-kill-ring)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x m") 'consult-bookmark)
(global-set-key (kbd "C-x f") 'consult-recent-file)
(define-key minibuffer-local-map (kbd "C-r") 'consult-history)


;; Set up Orderless for better fuzzy matching
(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

;; corfu configuration
(setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
(setq corfu-auto t)                 ;; Enable auto completion
;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
(setq corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
(setq corfu-quit-no-match t)        ;; Automatically quit if there is no match
(setq corfu-echo-documentation nil) ;; Do not show documentation in the echo area
;;start corfu
(global-corfu-mode)


;; Add `completion-at-point-functions', used by `completion-at-point'.
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-keyword)
;;(add-to-list 'completion-at-point-functions #'cape-ispell)

(setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(provide 'drm-completion)
