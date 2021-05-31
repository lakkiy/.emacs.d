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
(straight-use-package '(popper :type git :host github :repo "karthink/popper"))
(straight-use-package '(oca :type git :host github :repo "lepisma/oca"))
(straight-use-package '(devdocs-browser :type git :host github :repo "blahgeek/emacs-devdocs-browser"))
(straight-use-package 'sr-speedbar)
(straight-use-package 'notmuch)

;;; notmuch TODO
(setq
 notmuch-show-logo nil)

(autoload 'notmuch "notmuch" "notmuch mail" t)

;;; sr-speedbar
(setq
 ;; speed bar
 seedbar-use-images nil
 speedbar-show-unknown-files t
 speedbar-indentation-width 2
 ;; sr-speedbar
 sr-speedbar-default-width 20
 sr-speedbar-max-width 30
 sr-speedbar-right-side nil
 sr-speedbar-skip-other-window-p nil)

(global-set-key (kbd "<f8>") 'sr-speedbar-toggle)

;;; devdocs-browser
(global-set-key (kbd "C-c D") 'devdocs-browser-open)

;;; gnus TODO

;;; oca TODO

;;; popper
(setq
 popper-modeline nil
 popper-mode-line nil
 popper-reference-buffers
 '("\\*Messages\\*"
   "Outout\\*$"
   help-mode
   eshell-mode
   ielm-mode)
 popper-group-function #'popper-group-by-directory)

(autoload 'popper-toggle-latest "popper" nil t)
(autoload 'popper-cycle "popper" nil t)

(global-set-key (kbd "C-`") 'popper-toggle-latest)
(global-set-key (kbd "M-`") 'popper-cycle)

(add-hook 'after-init-hook 'popper-mode)

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
