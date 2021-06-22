;;; -*- lexical-binding: t -*-

(straight-use-package '(twidget :type git :host github :repo "Kinneyzhang/twidget"))
(straight-use-package '(svg-lib :type git :host github :repo "rougier/svg-lib"))
(straight-use-package 'ggtags)

(+pdump-packages 'twidget
                 'ggtags
                 'svg-lib)

;;; TODO ggtags

;;; TODO svg-lib

;; TODO twidget
(straight-use-package 'ov)

(provide 'init-fun)
