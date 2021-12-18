;;; -*- lexical-binding: t -*-

(eat-package rainbow-mode
  :straight t
  :commands rainbow-mode)

;; NOTE not work on macos with emacs-build
(eat-package screenshot
  :straight (screenshot :type git :host github :repo "tecosaur/screenshot"))

(eat-package find-orphan
  :straight (find-orphan
             :type git
             :host github
             :repo "manateelazycat/find-orphan")
  :commands
  find-orphan-function-in-buffer
  find-orphan-function-in-directory)

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

(straight-use-package 'dumb-jump)
(straight-use-package 'highlight-numbers)
(straight-use-package 'vterm)
(straight-use-package 'eldoc-overlay)

(provide 'init-fun)
