;; -*- lexical-binding: t; -*-

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(leaf rainbow-mode
  :doc "show ansi-color, enable manually"
  :straight t
  :commands
  (rainbow-mode))

(leaf hackernews
  :doc "can't open hackernews in company's network"
  :straight
  (hackernews :type git
              :host github
              :repo "clarete/hackernews.el")
  :commands
  (hackernews))

(leaf eaf
  :doc "monkeytype in company ("
  :straight
  (eaf :type git
       :host github
       :repo "manateelazycat/emacs-application-framework"
       :files ("*"))
  :init
  (leaf epc :straight t :leaf-defer t)
  (leaf ctable :straight t :leaf-defer t)
  (leaf deferred :straight t :leaf-defer t)
  (leaf s :straight t :leaf-defer t)
  :commands
  (eaf-open-browser eaf-open eaf-open-bookmark)
  :custom
  (browse-url-browser-function . 'eaf-open-browser)
  (eaf-browser-continue-where-left-off . t)
  :config
  (require 'eaf-org)
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-setq eaf-browser-enable-autofill "true"))

(provide 'init-unnecessary)
