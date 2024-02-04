;;; drm-key-bindings.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Set up some keybindings using general el.

;;; Code:

;; packages

(straight-use-package 'general)

;; unset some default bindings
(global-unset-key "\C-h")
(global-unset-key "\C-s")
(global-unset-key "\C-r")

;; config

(general-define-key
 :prefix "C-h"
 "f" '(helpful-function :wk "Describe function")
 "v" '(helpful-variable :wk "Describe variable")
 "k" '(helpful-key :wk "Describe key")
 "m" '(describe-mode :wk "Describe mode")
 "o" '(helpful-symbol :wk "Describe symbol")
 "p" '(describe-package :wk "Describe package")
 "x" '(helpful-command :wk "Describe command"))

(general-define-key
 :prefix "C-x"
 "o" '(iwindow-select :wk "Go to other window")
 "0" '(iwindow-delete :wk "Delete current window")
 "1" '(iwindow-delete-others :wk "Close all other windows")
 "s" '(iwindow-swap :wk "Swap current window other window")
 "3" '(split-window-right :wk "Split window horizontally")
 "2" '(split-window-below :wk "Split window vertically"))

(general-define-key
 :prefix "C-c"
 "t" '(:ignore t :wk "Toggle")
 "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
 "t t" '(visual-line-mode :wk "Toggle truncated lines")
 "t p" '(popper-toggle :wk "Toogle popper"))

(general-define-key
 :prefix "C-c"
 "p" '(:ignore t :wk "Popper")
 "p n" '(popper-cycle :wk "Cycle popper window")
 "p t" '(popper-toggle-type :wk "Toggle making the window a popup"))

(general-define-key
 :prefix "C-s"
 "s" '(isearch-forward :wk "isearch forwards")
 "b" '(isearch-backward :wk "isearch backwards")
 "r s" '(isearch-forward-regexp :wk "isearch forwards regexp")
 "r b" '(isearch-backward-regexp :wk "isearch backwards regexp")
 "o" '(occur :wk "Occur")
 "l" '(consult-line :wk "Consult line")
 "L" '(consult-line-multi :wk "Consult line multi")
 "O" '(consult-outline :wk "Consult outline")
 "i" '(consult-imenu :wk "Consult imenu")
 "I" `(consult-imenu-multi :wk "Consult imenu multi")
 "g" '(consult-goto-line :wk "Consult goto line")
 "k" '(consult-kmacro :wk "Consult kmacro")
 "f" '(consult-flymake :wk "Consult flymake")
 "m" '(consult-mark :wk "Consult mark"))

(general-define-key
 :prefix "C-r"
 "r" '(replace-regexp :wk "Replace regexp")
 "q" '(query-replace-regexp :wk "Query replace")
 "s" '(string-rectangle :wk "Replace rectangle content with STRING")
 "i" '(string-insert-rectangle :wk "Insert STRING on each line of rectangle"))

(general-define-key
 :prefix "C-;"
 ";" '(evilnc-comment-or-uncomment-lines :wk "Comment/Uncomment lines")
 "b" '(evilnc-comment-box :wk "Comment/Uncomment region and put it in a box")
 "l" '(evilnc-comment-or-uncomment-to-the-line :wk "Comment/Uncomment till line NUM")
 "t" '(evilnc-comment-or-uncomment-html-tag :wk "Comment/Uncomment HTML tag(s)")
 "k" '(evilnc-comment-and-kill-ring-save :wk "Comment and save to the kill ring"))


(provide 'drm-key-bindings)
;;; drm-key-bindings.el ends here
