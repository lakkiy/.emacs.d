;;; -*- lexical-binding: t -*-

(straight-use-package 'rainbow-mode)
(straight-use-package 'docstr)
(straight-use-package 'parrot)
(straight-use-package 'restclient)
(straight-use-package '(emacs-calfw :type git :host github :repo "kiwanami/emacs-calfw"))
(straight-use-package 'major-mode-hydra)

;; major-mode-hydra
;; TODO maybe just need pretty-hydra
;; TODO add to basic file
(global-set-key (kbd "<f6>") #'major-mode-hydra)
(autoload #'major-mode-hydra "major-mode-hydra" nil t)

;; emacs-calfw
(setq cfw:org-overwrite-default-keybinding t)

(autoload 'cfw:open-calendar-buffer "calfw" nil t)
(autoload 'cfw:open-org-calendar "calfw-org" nil t)

(with-eval-after-load "calfw"
  ;; SPC-SPC is used in Motion mode to run M-X
  (define-key cfw:calendar-mode-map (kbd "RET") 'cfw:show-details-command))

;; rainbow-mode
(autoload 'rainbow-mode "rainbow-mode" nil t)

;; docstr
;; FIXME go-mode, don't know how to use
(add-hook 'prog-mode-hook (lambda () (docstr-mode 1)))

;; TODO parrot
;; add to modeline

;; restclient
;; TODO find other packages like this or add to to normal config
(autoload 'restclient-mode "restclient" nil t)

(provide 'init-fun)
