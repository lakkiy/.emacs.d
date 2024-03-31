
(install-package 'app-launcher "https://github.com/SebastienWae/app-launcher")

(defun emacs-run-launcher ()
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
                    (minibuffer . only)
                    (fullscreen . 0) ; no fullscreen
                    (undecorated . t) ; remove title bar
                    ;;(auto-raise . t) ; focus on this frame
                    ;;(tool-bar-lines . 0)
                    ;;(menu-bar-lines . 0)
                    (internal-border-width . 10)
                    (width . 80)
                    (height . 11)))
    (unwind-protect
        (app-launcher-run-app)
      (delete-frame))))

(provide 'init-launcher)
