(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)

;; package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/langs" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/drm-functions/" user-emacs-directory))

;;load component modules
(require 'drm-ui)
(require 'drm-defaults)
(require 'drm-completion)
(require 'drm-windows)
(require 'drm-editing)
(require 'drm-productivity)
(require 'drm-programming)
(require 'drm-projects)
;; load my custom lisp modules
(require 'drm-dired-convert-with-pandoc)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
