(defun drm-tab-bar-tab-name-format (tab i)
  "format the name of the tab bar"
  (propertize
   (format
    (concat
     (if (eq (car tab) 'current-tab)
         " → " " ⊣ ")
     "%s")
    (alist-get 'name tab))))

(provide 'drm-tab-bar-tab-name-format)
