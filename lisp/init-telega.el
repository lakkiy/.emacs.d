;; -*- lexical-binding: t; -*-

(eat-package telega
  :straight (telega :tyep git
                    :host github
                    :repo "zevlg/telega.el")
  :commands telega
  :init
  (setq telega-chat-input-prompt "> "
        telega-animation-play-inline nil
        telega-video-play-inline nil
        ;; make sticker larger to read
        telega-sticker-size '(10 . 24)
        ;; change reply symbol
        telega-symbol-reply "↫"
        ;; set date format for old messages
        telega-old-date-format "%Y/%M/%D")

  ;; pgtk emacs does not support copying image from clipboard, a simple workaround
  (define-advice telega-chatbuf-attach-clipboard (:override (doc-p) yang)
    "Attach clipboard image to the chatbuf as photo.
If `\\[universal-argument]' is given, then attach clipboard as document."
    (interactive "P")
    (let* ((selection-coding-system 'no-conversion) ;for rawdata
           (temporary-file-directory telega-temp-dir)
           (tmpfile (telega-temp-name "clipboard" ".png"))
           (coding-system-for-write 'binary))
      (if sys/linuxp
          (shell-command-to-string
           (format "xclip -selection clipboard -t image/png -o > %s"
                   tmpfile))
        (write-region (or (gui-get-selection 'CLIPBOARD 'image/png)
                          (error "No image in CLIPBOARD"))
                      nil tmpfile nil 'quiet))
      (telega-chatbuf--attach-tmp-photo tmpfile doc-p)))

  (defun +telega-open-file (file)
    (cond
     ((member (downcase (file-name-extension file)) '("png" "jpg" "gif" "jpeg"))
      (start-process "telega-open-photo" nil "/sbin/imv" file))
     ((member (downcase (file-name-extension file)) '("mp4"))
      (start-process "telega-open-video" nil "/sbin/mpv" file))
     (t
      (find-file file))))
  :hook
  (telega-msg-ignore-predicates . telega-msg-from-blocked-sender-p)
  (telega-chat-mode . #'yas-minor-mode-on)
  :config
  ;; block user or set use id 1648334150(keke)
  (add-hook 'telega-msg-ignore-predicates 'telega-msg-from-blocked-sender-p)
  (when (not (display-graphic-p))
    (setq telega-open-message-as-file '(photo video)
          telega-open-file-function '+telega-open-file))
  ;; syntax highlighting in telega code
  (require 'telega-mnz)
  (global-telega-mnz-mode 1))

(provide 'init-telega)
