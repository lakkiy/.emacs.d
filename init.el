;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(add-hook 'after-init-hook
          (lambda ()
            (save-excursion
              (switch-to-buffer "*scratch*")
              (goto-char (point-min))
              (insert ";; \t\t\tE M A C S\n")
              (insert (format ";; %s\n" (make-string 58 ?-)))
              (insert (format ";; Welcome to GNU Emacs %s. "
                              emacs-version))
              (insert (format "Today is %s .\n"
                              (format-time-string "%A %Y.%-m.%-d")))
              (insert ";;\n")
              (lisp-interaction-mode)
              (goto-char (point-max)))))

;;; Personal configuration may override some variables
(let ((private-conf (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-conf)
    (load-file private-conf)))

(require 'init-basic)
(require 'init-hydra)
(require 'init-editor)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-meow)
(require 'init-rime)
(require 'init-ui)
(require 'init-completion)
(require 'init-git)
(require 'init-org)
(require 'init-window)
(require 'init-telega)
(require 'init-spcfile)
(require 'init-lisp)
(require 'init-mole)
(require 'init-mail)
(require 'init-dev)
(require 'init-fun)
