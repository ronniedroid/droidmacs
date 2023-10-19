;;; drm-key-bindings.el -*- lexical-binding: t; -*-

;; Author: Ronnie Nissan

;;; Commentary:

;; Set up some keybindings using general el.
;; Some lines of code were copied from distro tube's emacs config

;;; Code:

;; packages

(straight-use-package 'general)

;; config

;; set up 'SPC' as the global leader key
(general-create-definer drm/leader-keys
  :keymaps 'override
  :prefix "M-SPC") ;; set leader

(drm/leader-keys
  "b" '(:ignore t :wk "Buffer")
  "b b" '(consult-buffer :wk "Switch buffer")
  "b i" '(ibuffer :wk "Ibuffer")
  "b k" '(kill-this-buffer :wk "Kill this buffer")
  "b n" '(next-buffer :wk "Next buffer")
  "b p" '(previous-buffer :wk "Previous buffer")
  "b r" '(revert-buffer :wk "Reload buffer")
  "b s" '(save-buffer :wk "Save buffer")
  "b c" '(crdt-switch-to-buffer :wk "Switch to a crdt shared buffer"))

(drm/leader-keys
  "f" `(:ignore t :wk "File and Frame")
  "f f" '(find-file :wk "Find file")
  "f w" `(write-file :wk "Write file")
  "f i" `(insert-file :wk "Insert file")
  "f F" '(other-frame :wk "Go to other frame")
  "f d" '(delete-frame :wk "Delete current frame")
  "f o" '(delete-other-frames :wk "Close all other frames")
  "f c" `(clone-frame :wk "Clone current frame")
  "f r" `(set-frame-name :wk "Rename frame")
  "f p" '(other-frame-prefix :wk "Other frame prefix"))

(drm/leader-keys
  "w" '(:ignore t :wk "Window")
  "w w" '(other-window :wk "Go to other window")
  "w d" '(delete-window :wk "Delete current window")
  "w o" '(delete-other-windows :wk "Close all other windows")
  "w s" '(split-window-right :wk "Split window horizontally")
  "w v" '(split-window-below :wk "Split window vertically")
  "w h" '(evil-window-left :wk "Move to left window")
  "w j" '(evil-window-down :wk "Move to down window")
  "w k" '(evil-window-up :wk "Move to up window")
  "w l" '(evil-window-right :wk "Move to right window"))

(drm/leader-keys
  "e" '(:ignore t :wk "Evaluate")
  "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
  "e d" '(eval-defun :wk "Evaluate defun containing or after point")
  "e e" '(eval-expression :wk "Evaluate and elisp expression")
  "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
  "e r" '(eval-region :wk "Evaluate elisp in region"))

(drm/leader-keys
  "h" '(:ignore t :wk "Help")
  "h f" '(helpful-function :wk "Describe function")
  "h v" '(helpful-variable :wk "Describe variable")
  "h k" '(helpful-key :wk "Describe key")
  "h m" '(describe-mode :wk "Describe mode")
  "h o" '(helpful-symbol :wk "Describe symbol")
  "h p" '(describe-package :wk "Describe package")
  "h x" '(helpful-command :wk "Describe command"))

(drm/leader-keys
  "t" '(:ignore t :wk "Toggle")
  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  "t t" '(visual-line-mode :wk "Toggle truncated lines")
  "t p" '(popper-toggle-latest :wk "Toogle popper"))

(drm/leader-keys
  "p" '(:ignore t :wk "Project/popper")
  "p p" '(project-switch-project :wk "Switch project")
  "p b" '(consult-project-buffer :wk "Switch project buffer")
  "p f" '(project-find-file :wk "Project find file")
  "p d" '(project-find-dir :wk "Project find dir")
  "p g" '(project-find-regexp :wk "Project find regexp")
  "p D" '(project-dired :wk "Project dired")
  "p e" '(project-eshell :wk "Project eshell")
  "p s" '(project-shell :wk "Project shell")
  "p r" '(project-query-replace-regexp :wk "Project query replace regexp")
  "p k" '(project-kill-buffers :wk "Project kill all buffers")
  "p v" '(magit-status :wk "Open magit")
  "p n" '(popper-cycle :wk "Cycle popper window")
  "p t" '(popper-toggle-type :wk "Toggle making the window a popup"))

(drm/leader-keys
  "s" '(:ignore t :wk "Search and consult")
  "s s" '(isearch-forward :wk "isearch forwards")
  "s b" '(isearch-backward :wk "isearch backwards")
  "s r s" '(isearch-forward-regexp :wk "isearch forwards regexp")
  "s r b" '(isearch-backward-regexp :wk "isearch backwards regexp")
  "s o" '(occur :wk "Occur")
  "s l" '(consult-line :wk "Consult line")
  "s L" '(consult-line-multi :wk "Consult line multi")
  "s O" '(consult-outline :wk "Consult outline")
  "s i" '(consult-imenu :wk "Consult imenu")
  "s I" `(consult-imenu-multi :wk "Consult imenu multi")
  "s g" '(consult-goto-line :wk "Consult goto line")
  "s k" '(consult-kmacro :wk "Consult kmacro")
  "s f" '(consult-flymake :wk "Consult flymake")
  "s m" '(consult-mark :wk "Consult mark"))

(drm/leader-keys
  "r" '(:ignore t :wk "Replace")
  "r r" '(replace-regexp :wk "Replace regexp")
  "r q" '(query-replace :wk "Query replace"))

(drm/leader-keys
  "c" '(:ignore t :wk "Comment")
  "c c" '(evilnc-comment-or-uncomment-lines :wk "Comment/Uncomment lines")
  "c b" '(evilnc-comment-box :wk "Comment/Uncomment region and put it in a box")
  "c l" '(evilnc-comment-or-uncomment-to-the-line :wk "Comment/Uncomment till line NUM")
  "c t" '(evilnc-comment-or-uncomment-html-tag :wk "Comment/Uncomment HTML tag(s)")
  "c s" '(evilnc-comment-and-kill-ring-save :wk "Comment and save to the kill ring"))

(drm/leader-keys
  "x" `(:ignore t :wk "commands")
  "x x" `(execute-extended-command :wk "M-x")
  "x c" `(save-buffers-kill-terminal :wk "Exit emacs"))

(provide 'drm-key-bindings)
;;; drm-key-bindings.el ends here
