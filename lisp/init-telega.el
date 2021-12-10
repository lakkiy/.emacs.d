;; -*- lexical-binding: t; -*-

(eat-package telega
  :straight (telega :tyep git
                    :host github
                    :repo "zevlg/telega.el")
  :commands telega
  :init
  (defun +telega-font-setup ()
    (interactive)
    (setq buffer-face-mode-face `(:family ,+font-cn))
    (buffer-face-mode +1))
  :hook
  ;; ignore blocked user
  (telega-msg-ignore-predicates . telega-msg-from-blocked-sender-p)
  ;; font setup
  ((telega-root-mode-hook telega-chat-mode-hook) . +telega-font-setup)
  :config
  (setq telega-chat-input-prompt "> "
        telega-animation-play-inline nil
        telega-video-play-inline nil
        ;; make sticker larger to read
        telega-sticker-size '(10 . 24)
        ;; change reply symbol
        telega-symbol-reply "↫"
        ;; set date format for old messages
        telega-old-date-format "%Y/%M/%D")

  (custom-set-faces
   '(telega-entity-type-pre ((t :inherit 'fixed-pitch :family nil))))

  ;; syntax highlighting in telega code
  (require 'telega-mnz)
  (global-telega-mnz-mode 1))

(provide 'init-telega)
