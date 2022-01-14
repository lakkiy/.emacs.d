;;; -*- lexical-binding: t -*-

;; TODO remoce `dash.el'
(defun +project-previous-buffer (arg)
  "Toggle to the previous buffer that belongs to current project."
  (interactive "P")
  (if (equal '(4) arg)
      (if-let ((pr (project-current)))
          (switch-to-buffer
           (->> (project--buffer-list pr)
                (--remove (or (minibufferp it)
                              (get-buffer-window-list it)))
                (car))))
    (mode-line-other-buffer)))

(require 'init-meow-dvorak)
(require 'init-meow-qwerty)

(eat-package meow
  :straight t
  :hook
  (after-init-hook . (lambda ()
                       (meow-global-mode 1)))
  :config
  ;; SPC h f to describe-funtion
  (global-set-key (kbd "C-h C-f") 'describe-funtion)
  ;; normal mode list
  (dolist (mode '(go-dot-mod-mode
                  diff-mode))
    (add-to-list 'meow-mode-state-list `(,mode . normal)))
  ;; motion mode list
  (dolist (mode '(notmuch-hello-mode
                  notmuch-search-mode
                  notmuch-tree-mode))
    (add-to-list 'meow-mode-state-list `(,mode . motion)))
  ;; setup meow with layout
  (cond ((eq +meow-layout 'dvorak) (meow-setup-dvorak))
        (t (meow-setup-qwerty)))
  :init
  (setq meow-esc-delay 0.001))

(provide 'init-meow)
