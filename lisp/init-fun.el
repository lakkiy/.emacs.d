;;; -*- lexical-binding: t -*-


(straight-use-package 'ibuffer-vc)
(straight-use-package 'olivetti)
(straight-use-package 'rainbow-mode)
(straight-use-package 'docstr)
(straight-use-package 'parrot)
(straight-use-package '(vundo :type git :host github :repo "casouri/vundo"))
(straight-use-package 'vterm)
(straight-use-package 'leetcode)
(straight-use-package 'restclient)

;; olivetti
(autoload 'olivetti-mode "olivetti" nil t)

;; rainbow-mode
(autoload 'rainbow-mode "rainbow-mode")

;; docstr
(add-hook 'prog-mode-hook (lambda () (docstr-mode 1)))


;; TODO parrot
;; add to modeline

;; vundo
;; TODO add to normal config file
(autoload 'vundo "vundo" nil t)

;; vterm
(autoload 'vterm "vterm" nil t)

;; leetcode
;; TODO add to normal config
(setq
 leetcode-prefer-language "golang"
 leetcode-prefer-sql "mysql"
 leetcode-save-solutions t
 leetcode-directory "~/Dropbox/leetcode")

(autoload 'leetcode "leetcode" nil t)

;; restclient
;; TODO find other packages like this or add to to normal config
(autoload 'restclient-mode "restclient" nil t)

(provide 'init-fun)
