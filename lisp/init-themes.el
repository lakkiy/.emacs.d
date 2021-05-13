;;; -*- lexical-binding: t -*-

(straight-use-package '(lazycat-theme :type git :host github :repo "404cn/lazycat-theme"))
(straight-use-package '(modus-theme   :type git :host github :repo "protesilaos/modus-themes"))

(setq modus-themes-slanted-constructs t
      modus-themes-bold-constructs t
      modus-themes-syntax 'green-strings
      modus-themes-no-mixed-fonts t
      modus-themes-paren-match 'intense-bold)

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

(load-theme footheme t)

(provide 'init-themes)
