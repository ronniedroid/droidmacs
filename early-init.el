;;; early-init.el -*- lexical-binding: t; -*-

;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Prefer loading newest compiled .el file
(setq load-prefer-newer noninteractive)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Remove some unneeded UI elements
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; set the frame settings here
(setq default-frame-alist `(
			    (vertical-scroll-bars . nil)
			    (horizontal-scroll-bars . nil)
                            (fullscreen . maximized)
                            (undecorated . t)
			    (menu-bar-lines . 0)
			    (tool-bar-lines . 0)
			    (background-color . "white")
			    (foreground-color . "black")
			    (mouse-color . "black")
			    ))

;; Add frame borders
(setq-default left-margin-width 2)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;; housekeeping
(setq backup-directory-alist '(("." . "~/.cache/emacssaves")))
(setq create-lockfiles nil)
