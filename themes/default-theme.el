(deftheme default "A better default emacs theme.")

(let ((bg "#c4d3cd")
      (fg "#353535")
      (kw "#000000")
      (cm "#61726b")
      (ss "#96b0a5")
      (hl "#a9bdb5")
      (fn "#b6c6c0")
      (cur "#202020"))
  (custom-theme-set-faces
   `default
   ;; basic
   `(fringe ((t (:background ,(face-background 'default)))))

   ;; mode-line
   `(mode-line ((t (:foreground "black" :background "grey75" :box (:line-width 1 :style released-button)))))
   `(mode-line-inactive ((t (:foreground "grey20" :background "grey90" :weight light
                                         :box `(:line-width 1 :color "grey75" :style nil)))))
   ))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'default)
