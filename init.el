;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'init-basic)
(require 'init-editor)
(require 'init-doglooksgood)
(require 'init-ui)
(require 'init-completion)
(require 'init-git)
(require 'init-org)
(require 'init-window)
(require 'init-chat)
(require 'init-spcfile)
(require 'init-lisp)
(require 'init-mole)

(require 'init-lsp)
(require 'init-go)

(require 'init-fun)
