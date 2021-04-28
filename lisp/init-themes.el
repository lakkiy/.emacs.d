;;; -*- lexical-binding: t -*-

(straight-use-package '(nasy-theme    :type git :host github :repo "404cn/nasy-theme.el"))
(straight-use-package '(lazycat-theme :type git :host github :repo "404cn/lazycat-theme"))
(straight-use-package '(modus-theme   :type git :host github :repo "protesilaos/modus-themes"))
(straight-use-package '(joker-theme   :type git :host github :repo "DogLooksGood/joker-theme"))

(require 'joker-theme)

(load-theme 'lazycat-dark t)

(provide 'init-themes)
