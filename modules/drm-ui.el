;;; drm-ui.el -*- lexical-binding: t; -*-

;; some defaults
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)
(global-display-line-numbers-mode)

(set-face-attribute 'default nil
		    :font "JetBrainsMono Nerd Font Mono"
		    :height 120
		    :weight 'regular)

(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)
(straight-use-package 'helpful)
(straight-use-package 'which-key)
(straight-use-package 'page-break-lines)
(straight-use-package 'dashboard)
(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el" ))

;; theme
(setq modus-themes-hl-line '(intense))
(load-theme 'modus-operandi t)

;; dashboard
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-banner-logo-title "")
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-projects-backend 'project-el)
(setq dashboard-items '((recents  . 5)
  			(projects . 5)))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-init-info t)
(add-hook 'dashboard-mode-hook  '(lambda () (setq global-hl-line-mode nil)))
(dashboard-setup-startup-hook)

(with-eval-after-load 'dashboard
  (page-break-lines-mode))

;; enable which key for keybinding hinting
(which-key-mode)


;; Start up the modeline after initialization is finished
(add-hook 'after-init-hook 'doom-modeline-init)


;; Configure `doom-modeline'
(setq doom-modeline-height 15
      doom-modeline-bar-width 6
      doom-modeline-minor-modes t
      doom-modeline-buffer-file-name-style 'truncate-except-project)

;; Make `describe-*' screens more helpful!
(require 'helpful)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

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
