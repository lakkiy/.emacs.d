;;; -*- lexical-binding: t -*-

(straight-use-package '(nasy-theme    :type git :host github :repo "404cn/nasy-theme.el"))
(straight-use-package '(lazycat-theme :type git :host github :repo "404cn/lazycat-theme"))
(straight-use-package '(modus-theme   :type git :host github :repo "protesilaos/modus-themes"))
(straight-use-package '(joker-theme   :type git :host github :repo "DogLooksGood/joker-theme"))

(require 'joker-theme)

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

;;; Margin
(let ((margin 0))
  (add-to-list 'default-frame-alist (cons 'internal-border-width margin)))

;;; Transparency
(let ((alpha 100))
  (add-to-list 'default-frame-alist (cons 'alpha alpha)))

;;; No window decoration
(add-to-list 'default-frame-alist (cons 'undecorated t))

(load-theme 'joker t)

(provide 'init-themes)
