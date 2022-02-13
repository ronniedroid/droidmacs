;;; drm-windows.el -*- lexical-binding: t; -*-

(straight-use-package 'shackle)
(straight-use-package 'popper)

;; shackle configurations
(setq shackle-lighter "")
(setq shackle-select-reused-windows nil) ; default nil
(setq shackle-default-alignment 'below) ; default below
(setq shackle-default-size 0.4) ; default 0.5
;; Shackle window rules
(setq shackle-rules
      ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
      '((compilation-mode              :select nil)
        ("*eshell*"                    :select t                          :align 'below :size 0.4 :popup t)
        ("*[[:word:]]+-eshell*"        :select t                          :align 'below :size 0.4 :popup t :regexp t)
        ("*Shell Command Output*"      :select nil)
        ("*Help*"                      :select t                          :align 'right :size 0.4 :popup t)
        (Helpful-mode                  :select t                          :align 'right :size 0.4 :popup t :regexp t)
        ("*Org Export Dispatcher*"     :select t   :inhibit-window-quit t :align 'right :size 0.4 :popup t)
        ("*Completions*"                                                  :align t     :size 0.3)
        ("*Messages*"                  :select nil :inhibit-window-quit nil :align 'right :size 0.3 :popup t)
        ("*info*"                      :select t   :inhibit-window-quit t                        :same t)
        ))
;;enable Shackle
(shackle-mode 1)

(global-set-key (kbd "C-c p p") 'popper-toggle-latest)
(global-set-key (kbd "C-c p n") 'popper-cycle)
(global-set-key (kbd "C-c p t") 'popper-toggle-type)
(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Warnings\\*"
        "-eshell\\*$"
        eshell-mode
        help-mode
        Helpful-mode
        compilation-mode))
(setq popper-display-control nil)
;; start popper
(popper-mode +1)

(provide 'drm-windows)
