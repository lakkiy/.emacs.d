;;; -*- lexical-binding: t -*-

(straight-use-package 'rainbow-mode)
(straight-use-package 'docstr)
(straight-use-package 'restclient)
(straight-use-package '(emacs-calfw :type git :host github :repo "kiwanami/emacs-calfw"))
(straight-use-package '(twidget :type git :host github :repo "Kinneyzhang/twidget"))
(straight-use-package '(emacs-application-framework
                        :type git
                        :host github
                        :repo "manateelazycat/emacs-application-framework"
                        :files ("*")))
(straight-use-package 'pretty-hydra)
(straight-use-package 'emojify)

;;; emojify
(add-hook 'after-init-hook #'global-emojify-mode)

;;; eaf
(straight-use-package 'epc)
(straight-use-package 'deferred)
(straight-use-package 'ctable)

(setq
 eaf-browser-continue-where-left-off t)

(with-eval-after-load "eaf"
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding))

;; TODO twidget
(straight-use-package 'ov)

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

;; restclient
(autoload 'restclient-mode "restclient" nil t)

(defun my/tab-bar-new-restclient-tab ()
  (interactive)
  (let ((inhibit-message t))
    (tab-bar-new-tab)
    (tab-bar-rename-tab "*restclient*")
    (my/restclient-new)))

(defun my/restclient-new ()
  (interactive)
  (get-buffer-create "*restclient*")
  (switch-to-buffer "*restclient*")
  (with-current-buffer "*restclient*"
    (restclient-mode)))


(provide 'init-fun)
