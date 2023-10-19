;; drm-ui.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; packages and configuration to make emacs' UI nicer and more functional

;;; Code:

;; packages

(straight-use-package 'dashboard)
(straight-use-package 'echo-bell)
(straight-use-package 'helpful)
(straight-use-package 'page-break-lines)
(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el" ))
(straight-use-package 'modus-themes)

;; config

;; Some defaults
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda () (setq left-margin-width 0)))
(setq echo-bell-string "🔔")
(setq echo-bell-background "white")
(echo-bell-mode 1)
(pixel-scroll-precision-mode 1)

;; Defaults font
(set-face-attribute 'default nil
                    :font "JetBrains Mono Nerd Font Mono"
                    :height 110
                    :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :font "Inter"
                    :height 120
                    :weight 'regular)
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono Nerd Font Mono"
                    :height 110
                    :weight 'regular)

;; Set default theme
(setq modus-vivendi-palette-overrides
      '((bg-main "#1E1E1E")))

(load-theme 'modus-vivendi-tinted t)
(setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
(global-set-key [f5] #'modus-themes-toggle)

;; Dahboard
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-banner-logo-title "DroidMacs")
(setq dashboard-startup-banner 'official)
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-projects-backend 'project-el)
(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (bookmarks . 5)))
(setq dashboard-set-heading-icons nil)
(setq dashboard-set-file-icons nil)
(setq dashboard-set-init-info nil)
(setq dashboard-set-footer nil)
(add-hook 'dashboard-mode-hook  #'(lambda () (setq global-hl-line-mode nil)))
(dashboard-setup-startup-hook)

(with-eval-after-load 'dashboard
  (page-break-lines-mode))

;;; Modeline
;; Require my customized mode-line
(require 'drm-mode-line)

;; Make `describe-*' screens more helpful!
(require 'helpful)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key [remap describe-key] #'helpful-key)

;; Ligatures for the current font
(ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                     "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                     "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                     "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                     "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                     "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                     ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                     "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                     "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                     "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                     "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
(global-ligature-mode t)

(provide 'drm-ui)
;;; drm-ui.el ends here
