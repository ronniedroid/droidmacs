(require 'drm-ui)
(require 'drm-defaults)
(require 'drm-completion)
(require 'drm-windows)
(require 'drm-editing)
(require 'drm-productivity)
(require 'drm-programming)
(require 'drm-dired-convert-with-pandoc)

(straight-use-package 'project)

(require 'project)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c c") 'rn/convert-with-pandoc)
  )

(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "g") 'magit-status)
  (define-key project-prefix-map (kbd "G") 'project-find-regexp))

(setq project-switch-commands
      (append '((magit-status "Magit status"))
              project-switch-commands))
