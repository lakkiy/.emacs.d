;;; -*- lexical-binding: t -*-

(straight-use-package 'rainbow-mode)
(straight-use-package 'docstr)
(straight-use-package 'restclient)
(straight-use-package '(emacs-calfw :type git :host github :repo "kiwanami/emacs-calfw"))
(straight-use-package '(twidget :type git :host github :repo "Kinneyzhang/twidget"))
(straight-use-package '(popper :type git :host github :repo "karthink/popper"))
(straight-use-package '(oca :type git :host github :repo "lepisma/oca"))
(straight-use-package '(devdocs :type git :host github :repo "astoff/devdocs.el"))
(straight-use-package 'sr-speedbar)
(straight-use-package 'esup)
(straight-use-package 'vterm)
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package '(svg-lib :type git :host github :repo "rougier/svg-lib"))

(+pdump-packages 'rainbow-mode
                 'vterm
                 'tree-sitter
                 'tree-sitter-langs
                 'docstr
                 'esup
                 'restclient
                 'calfw
                 'twidget
                 'popper
                 'oca
                 'devdocs-browser
                 'sr-speedbar
                 'svg-lib
                 'notmuch)

;;; TODO svg-lib

;;; tree-sitter
;; (add-hook 'go-mode-hook 'tree-sitter-mode)
;; (add-hook 'go-mode-hook 'tree-sitter-hl-mode)

(with-eval-after-load "tree-sitter"
  (require 'tree-sitter-langs))

;;; sr-speedbar
(setq
 ;; speed bar
 speedbar-use-images nil
 speedbar-show-unknown-files t
 speedbar-indentation-width 2
 ;; sr-speedbar
 sr-speedbar-auto-refresh nil
 sr-speedbar-default-width 30
 sr-speedbar-max-width 40
 sr-speedbar-right-side nil
 sr-speedbar-skip-other-window-p t)

(defun +sr-speedbar-select-or-open()
  "Select sr-speedbar window if it exist, or open sr-speedbar"
  (interactive)
  (if (and (fboundp 'sr-speedbar-exist-p) (sr-speedbar-exist-p))
      (sr-speedbar-select-window)
    (sr-speedbar-open)))

(global-set-key (kbd "<f1>") #'+sr-speedbar-select-or-open)

(with-eval-after-load "sr-speedbar"
  (define-key speedbar-mode-map (kbd "<f1>") 'sr-speedbar-close)

  (defun speedbar-set-mode-line-format ()
    "Disable mode line and header line in speedbar"
    (setq mode-line-format nil)
    (setq header-line-format nil))

  (add-hook 'speedbar-mode-hook (lambda ()
                                     (face-remap-add-relative 'default :height 0.8)
                                     (face-remap-add-relative 'hl-line :box '(:line-width (-1 . -1))))))

;;; devdocs
(global-set-key (kbd "C-c b") 'devdocs-lookup)

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
