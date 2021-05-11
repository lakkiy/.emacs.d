;;; -*- lexical-binding: t -*-

(straight-use-package '(lazycat-theme :type git :host github :repo "404cn/lazycat-theme"))
(straight-use-package '(modus-theme   :type git :host github :repo "protesilaos/modus-themes"))

;;; Use window divider
(window-divider-mode 1)
;;; Nice window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

;;; No cursor blink
(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

;;; No fringe in minibuffer
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))

(load-theme 'lazycat-dark t)

(provide 'init-themes)
