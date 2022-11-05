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

(defun drm-completion/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

(require 'vertico)
;;(require 'vertico-directory)

;; Cycle back to top/bottom result when the edge is reached
(setq vertico-cycle nil)

;; Start Vertico
(vertico-mode 1)

;; start savehist
(savehist-mode)

;; Configure Marginalia
(require 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

;; Set some consult bindings
(global-set-key (kbd "C-s k") 'consult-kmacro)
(global-set-key (kbd "C-s f") 'consult-flymake)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-s g") 'consult-goto-line)
(global-set-key (kbd "C-s o") 'consult-outline)
(global-set-key (kbd "C-s m") 'consult-mark)
(global-set-key (kbd "C-s l") 'consult-line)
(global-set-key (kbd "M-y") 'consult-yank-pop)
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
;;(add-to-list 'completion-at-point-functions #'cape-tex)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-keyword)
;;(add-to-list 'completion-at-point-functions #'cape-ispell)
;;(add-to-list 'completion-at-point-functions #'cape-symbol)
;;(add-to-list 'completion-at-point-functions #'cape-line)

(setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(provide 'drm-completion)
