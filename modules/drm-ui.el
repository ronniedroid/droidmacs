;;; drm-ui.el -*- lexical-binding: t; -*-

;; Packages
(straight-use-package 'dashboard)
(straight-use-package 'echo-bell)
(straight-use-package 'dim)
(straight-use-package 'helpful)
(straight-use-package 'page-break-lines)
(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el" ))

;; Some defaults
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda () (setq left-margin-width 0)))
(setq echo-bell-string "🔔")
(setq echo-bell-background "white")
(echo-bell-mode 1)


;; Defaults font
(set-face-attribute 'default nil
		    :font "JetBrainsMono Nerd Font Mono"
		    :height 120
		    :weight 'regular)

;; Modus themes settings
(setq modus-themes-syntax '(yellow-comments green-strings))
(setq modus-themes-mode-line '(borderless accented))
(setq modus-themes-region '(bg-only))
(setq modus-themes-completions '((matches . (extrabold underline))
                                 (selection .(semibold accented))))
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-paren-match '(bold))
(setq modus-themes-hl-line '(accented))
(setq modus-themes-prompts '(intense bold))

(setq modus-themes-headings
      (quote ((0 . (rainbow))
              (1 . (rainbow extrabold overline))
              (2 . (rainbow))
              (3 . (rainbow))
              (t . (rainbow)))))

(setq modus-themes-org-blocks 'tinted-background)

(global-set-key [f5] #'modus-themes-toggle)

;; Set default theme
(load-theme 'modus-operandi t)

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

;;; Tab bar mode settings
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-tab-hints nil)

(setq tab-bar-format '(tab-bar-format-tabs
                       tab-bar-separator
                       tab-bar-format-align-right
                       tab-bar-format-global))
(setq tab-bar-menu-bar-button " 𝛌")
(setq battery-mode-line-format "⌁%p%% ")
(display-battery-mode)
(display-time-mode)

;; Setting keys to switch between tabs
(global-set-key (kbd "M-1") 'tab-bar-select-tab)
(global-set-key (kbd "M-2") 'tab-bar-select-tab)
(global-set-key (kbd "M-3") 'tab-bar-select-tab)
(global-set-key (kbd "M-4") 'tab-bar-select-tab)
(global-set-key (kbd "M-5") 'tab-bar-select-tab)

(defun drm-set-tab-bar-colors ()
  "Set the tab-bar colors to match the mode-line"
  (interactive)
  (let ((mode-line-bg (face-attribute 'mode-line :background))
        (mode-line-fg (face-attribute 'mode-line :foreground)))
    (custom-set-faces
     `(tab-bar ((t :background ,mode-line-bg :foreground ,mode-line-fg))))))

(add-hook 'modus-themes-after-load-theme-hook #'drm-set-tab-bar-colors)
(add-hook 'after-init-hook #'drm-set-tab-bar-colors)

;; Remape the default key to switch between tabs
(global-set-key (kbd "C-x t t") 'tab-switch)

;; Changint the tab-bar name and format
(require 'drm-tab-bar-tab-name-current-or-project)
(require 'drm-tab-bar-tab-name-format)
(setq tab-bar-tab-name-function #'drm-tab-bar-tab-name-current-or-project)
(setq tab-bar-tab-name-format-function #'drm-tab-bar-tab-name-format)

;; Activate tab-bar
(tab-bar-mode 1)

;;; Modeline

;; Require my customized mode-line
(require 'drm-mode-line)

;; Disable flymake title in modeline (only show counters)
(setq flymake-mode-line-title " ⚑")

;; Correctly show the underlines when the padding is increased
(setq x-underline-at-descent-line t)
;; Enable column number in the modline and change the position construct format
(column-number-mode t)
(setq mode-line-position (list "%l:%C"))

;; Add directory name to files with the same name
(setq uniquify-buffer-name-style 'forward)
(require 'uniquify)

;; Dim settings, hide or change how major and minor modes show in the modline
(dim-major-names
 '((emacs-lisp-mode           "ELISP")
   (inferior-emacs-lisp-mode  "ELISP<")
   (clojure-mode              "CLJ")
   (dashboard-mode             "Dashboard")
   (clojurescript-mode         "CLJS")))

(dim-minor-names
 '((visual-line-mode   " ↩")
   (auto-fill-function " ↵")
   (eldoc-mode         ""    eldoc)
   (ws-butler-mode    ""  ws-butler)
   (format-all-mode   "  "  format-all)
   (which-key-mode "" which-key)
   (emmet-mode     ""  emmet)
   (page-break-lines-mode "" page-break-lines)
   (subword-mode   ""  subword)))

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
