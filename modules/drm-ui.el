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
(straight-use-package 'dim)
(straight-use-package 'helpful)
(straight-use-package 'which-key)
(straight-use-package 'page-break-lines)
(straight-use-package 'dashboard)
(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el" ))

;; theme
(setq modus-themes-hl-line '(accented))
(setq modus-themes-mode-line '(accented 4))
(setq modus-themes-region '(bg-only))
(setq modus-themes-syntax '(green-strings yellow-comments))
(setq modus-themes-bold-constructs t)
(setq modus-themes-paren-match '(bold intense))
(setq modus-themes-italic-constructs t)
(setq modus-themes-completions '((matches . (extrabold intense))
                                 (selection . (accented))))
(load-theme 'modus-operandi t)
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

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
(add-hook 'dashboard-mode-hook  #'(lambda () (setq global-hl-line-mode nil)))
(dashboard-setup-startup-hook)

(with-eval-after-load 'dashboard
  (page-break-lines-mode))

;; enable which key for keybinding hinting
(which-key-mode)

;;; modeline

;; function to split modeline into left and right sides
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

;; set what the modeline should show and in what order
(modus-themes-with-colors
  (setq-default mode-line-format
                '((:eval
                   (simple-mode-line-render
                    ;; Left.
                    (quote ("%e"
                            mode-line-front-space
                            mode-line-modified
                            mode-line-frame-identification
                            mode-line-buffer-identification
                            " "
                            mode-line-position
                            " "
                            (vc-mode vc-mode)))
                    ;; Right.
                    (quote (""
                            mode-line-modes
                            " "
                            mode-line-misc-info
                            (dashboard-mode display-battery-mode)
                            (dashboard-mode display-time-mode)
                            mode-line-end-spaces)))))))


;; change how the mode-line-modes is displayed in the modeline
(modus-themes-with-colors
  (setq mode-line-modes
        (list
         `(:propertize ("" minor-mode-alist)
		       mouse-face mode-line-highlight
		       local-map ,mode-line-minor-mode-keymap)
         " "
         `(:propertize ("" mode-name)
                       'face 'bold
	               mouse-face mode-line-highlight
	               local-map ,mode-line-major-mode-keymap)
         '("" mode-line-process))))

;; disable flymake title in modeline (only show counters)
(setq flymake-mode-line-title "fm")

;; correctly show the underlines when the padding is increased
(setq x-underline-at-descent-line t)
;; enable column number in the modline and change the position construct format
(column-number-mode t)
(setq mode-line-position (list "%l:%C"))

;; add directory to name to files with the same name
(setq uniquify-buffer-name-style 'forward)
(require 'uniquify)

;; dim settings, hide or change how major and minor modes show in the modline
(dim-major-names
 '((emacs-lisp-mode           "EL")
   (inferior-emacs-lisp-mode  "EL<")
   (clojure-mode              "CLJ")
   (dashboard-mode             "")
   (clojurescript-mode         "CLJS")))

(dim-minor-names
 '((visual-line-mode   " ↩")
   (auto-fill-function " ↵")
   (eldoc-mode         ""    eldoc)
   (ws-butler-mode    ""  ws-butler)
   (format-all-mode   " fmt"  format-all)
   (which-key-mode "" which-key)
   (lsp-lens-mode  "" lsp-lens)
   (emmet-mode     ""  emmet)
   (page-break-lines-mode "" page-break-lines)
   (subword-mode   ""  subword)))

;; Make `describe-*' screens more helpful!
(require 'helpful)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key [remap describe-key] #'helpful-key)

;; ligatures for the current font
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
