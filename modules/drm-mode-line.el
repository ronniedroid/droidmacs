;;; drm-mode-line.el -*- lexical-binding: t; -*-

;; require a function from ./drm-functions to split modeline into left and right sides
(require 'drm-simple-mode-line-render)

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

(provide 'drm-mode-line)
