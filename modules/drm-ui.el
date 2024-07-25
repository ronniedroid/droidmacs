;; drm-ui.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; packages and configuration to make emacs' UI nicer and more functional

;;; Code:

;; Some defaults
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda () (setq left-margin-width 0)))
(pixel-scroll-precision-mode 1)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Defaults font
(set-face-attribute 'default nil
                    :font "JetBrains Mono Nerd Font Mono"
                    :height 105
                    :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :font "Inter"
                    :height 120
                    :weight 'regular)
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono Nerd Font Mono"
                    :height 105
                    :weight 'regular)

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
  :bind ([f5] . #'modus-themes-toggle)
  :init
  (load-theme 'modus-operandi-tinted t))

;; Dahboard
(use-package dashboard
  :custom
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-banner-logo-title "DroidMacs")
  (dashboard-startup-banner 'official)
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (bookmarks . 5)))
  (dashboard-set-heading-icons nil)
  (dashboard-set-file-icons nil)
  (dashboard-set-init-info nil)
  (dashboard-set-footer nil)
  :init
  (add-hook 'dashboard-mode-hook  #'(lambda () (setq global-hl-line-mode nil)))
  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :after dashboard)

;;; Modeline
;; Require my customized mode-line
(require 'drm-mode-line)

;; Make `describe-*' screens more helpful!
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                                       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                                       "<:<" ";;;"))
  (global-ligature-mode t))

(provide 'drm-ui)
;;; drm-ui.el ends here
