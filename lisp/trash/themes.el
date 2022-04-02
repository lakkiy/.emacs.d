;; `the-matrix-theme'
(straight-use-package 'the-matrix-theme)
;; `acme-theme'
(straight-use-package 'acme-theme)

;; `atom-one-dark-theme'
(straight-use-package 'atom-one-dark-theme)

(setf (alist-get +font-unicode face-font-rescale-alist 0.7 nil 'string=) 0.7)
(setf (alist-get +font-variable-pitch face-font-rescale-alist 1.3 nil 'string=) 1.3)

(set-frame-parameter nil 'internal-border-width 10)
(setq-default left-margin-width 0 right-margin-width 2)
(set-window-margins nil 0 0)

(straight-use-package 'kuronami-theme)
