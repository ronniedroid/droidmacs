;;; drm-mode-line.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; packages and configurations related to the mode line

;;; Code:

;; packages

(straight-use-package 'dim)

;; config

(defun drm-simple-mode-line-render (left right)
  "Return a string of `window-width' length.
  Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default mode-line-format
              '((:eval
                 (drm-simple-mode-line-render
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
                          mode-line-end-spaces))))))


;; change how the mode-line-modes is displayed in the modeline
(setq mode-line-modes
      (list
       `(:propertize ("" minor-mode-alist)
		     mouse-face mode-line-highlight
		     local-map ,mode-line-minor-mode-keymap)
       " "
       `(:propertize ("[" mode-name "]")
                     face bold
	             mouse-face mode-line-highlight
	             local-map ,mode-line-major-mode-keymap)
       '("" mode-line-process)))

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

(provide 'drm-mode-line)
;;; drm-mode-line.el ends here
