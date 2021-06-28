;;; -*- lexical-binding: t -*-

(straight-use-package '(twidget :type git :host github :repo "Kinneyzhang/twidget"))
(straight-use-package '(svg-lib :type git :host github :repo "rougier/svg-lib"))

(+pdump-packages 'twidget
                 'svg-lib)


;;; TODO svg-lib

;; TODO twidget
(straight-use-package 'ov)

(provide 'init-fun)
