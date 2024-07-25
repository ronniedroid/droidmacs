(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)

;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(require 'use-package)
(setq use-package-always-ensure t)

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;;load component modules
(require 'drm-ui)
(require 'drm-defaults)
(require 'drm-windows)
(require 'drm-completion)
(require 'drm-editing)
(require 'drm-productivity)
(require 'drm-programming)
(require 'drm-project)
(require 'drm-wk)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-fold-catch-invisible-edits 'show-and-error nil nil "Customized with use-package org-modern")
 '(package-selected-packages
   '(cider js2-mode rainbow-delimiters exec-path-from-shell format-all magit which-key consult-notes denote markdown-mode org-modern smartparens ws-butler evil-nerd-commenter popper shackle no-littering ligature echo-bell helpful page-break-lines dashboard modus-themes kind-icon cape corfu orderless vertico marginalia consult)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
