;;; drm-clojure.el -*- lexical-binding: t; -*-

(straight-use-package 'clojure-mode)
(straight-use-package 'clojure-mode-extra-font-locking)
(straight-use-package 'flymake-kondor)
(straight-use-package 'cider)

(require 'clojure-mode-extra-font-locking)

;; clojure mode configuration
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'flymake-kondor-setup)

(global-set-key ( kbd "C-c C-j") 'cider-jack-in)

;; cider repl configuration

(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(setq cider-repl-wrap-history t)
(setq cider-repl-history-file "~/.config/emacs/cider-history")
(setq cider-save-file-on-load t)
(setq cider-font-lock-dynamically '(macro core function var))
(setq cider-repl-display-help-banner nil)

(require 'cider)

(cider-register-cljs-repl-type 'nbb-or-scittle-or-joyride "(+ 1 2 3)")

(defun mm/cider-connected-hook ()
  (when (eq 'nbb-or-scittle-or-joyride cider-cljs-repl-type)
    (setq-local cider-show-error-buffer nil)
    (cider-set-repl-type 'cljs)))

(add-hook 'cider-connected-hook #'mm/cider-connected-hook)

(defun mm/cider-jack-in-nbb ()
  "Start a nbb nrepl process and connect."
  (interactive)
  (let* ((cider-allow-jack-in-without-project t)
	 (orig-buffer (current-buffer))
	 (params '(:jack-in-cmd "nbb nrepl-server :port 0"
				:cljs-repl-type nbb-or-scittle-or-joyride))
	 (params (cider--update-project-dir
		  params)))
    (nrepl-start-server-process
     (plist-get params :project-dir)
     (plist-get params :jack-in-cmd)
     (lambda (server-buffer)
       (with-current-buffer
	   orig-buffer
	 (cider-connect-sibling-cljs
	  params
	  server-buffer))))))


;;; FIXME: https://github.com/clojure-emacs/cider/issues/3255
(defun cider-verify-clojurescript-is-present ()
  "Check whether ClojureScript is present."
  (unless (nrepl-dict-get (cider-sync-tooling-eval "cljs.core/inc") "value")
    (user-error "ClojureScript is not available.  See https://docs.cider.mx/cider/basics/clojurescript for details")))


(add-hook 'clojure-mode-hook #'eglot-ensure)
;; (add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)

(provide 'drm-clojure)
