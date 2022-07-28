(deftheme default "Emacs default theme, better version.")

(custom-theme-set-faces
 `default
 ;; basic
 `(fringe ((t (:background ,(face-background 'default)))))
 `(cursor ((t (:background "black"))))

 ;; mode-line
 `(mode-line ((t (:foreground "black" :background "grey75" :box (:line-width 1 :style released-button)))))
 `(mode-line-inactive ((t (:foreground "grey20" :background "grey90" :weight light
                                       :box `(:line-width 1 :color "grey75" :style nil)))))
 )

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'default)
