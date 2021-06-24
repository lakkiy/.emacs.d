;; -*- lexical-binding: t; -*-

(straight-use-package '(telega :type git :host github :branch "releases"))

(+pdump-packages 'telega)

;;; telega -- many config from shenglao
(setq
 telega-chat-input-prompt "> "
 telega-animation-play-inline nil
 telega-video-play-inline nil
 ;; make sticker larger to read
 telega-sticker-size '(10 . 24)
 ;; change reply symbol
 telega-symbol-reply "↫"
 ;; set date format for old messages
 telega-old-date-format "%Y/%M/%D"
 telega-proxies '((:server "localhost" :port 7890 :enable t :type (:@type "proxyTypeSocks5"))))

(autoload #'telega "telega" nil t)

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

(defun yang/fixed-pitch-setup ()
  (interactive)
  ;; TODO text-scale-increase is nice for look
  (setq buffer-face-mode-face '(:family "Sarasa Mono SC"))
  (buffer-face-mode +1))

(with-eval-after-load "telega"
  (setq writeroom-width 82)
  (add-hook 'telega-root-mode-hook 'writeroom-mode)
  (add-hook 'telega-chat-mode-hook 'writeroom-mode)
  (add-hook 'telega-root-mode-hook #'yang/fixed-pitch-setup)
  (add-hook 'telega-chat-mode-hook #'yang/fixed-pitch-setup)
  ;; ignore blocked user
  (add-hook 'telega-msg-ignore-predicates 'telega-msg-from-blocked-sender-p)
  (add-hook 'telega-chat-mode #'yas-minor-mode-on)

  ;; syntax highlighting in telega code
  (require 'telega-mnz)
  (global-telega-mnz-mode 1))

;;; erc
(setq
 erc-server "irc.ea.libera.chat"
 erc-nick "Artorias"
 erc-user-full-name "Liu Bo"
 erc-rename-buffers t
 erc-interpret-mirc-color t
 erc-lurker-hide-list '("JOIN" "PART" "QUIT")
 erc-auto-query 'bury
 erc-kill-buffer-on-part t
 erc-autojoin-channels-alist '(("irc.ea.libera.chat" "#emacs"))
 erc-track-exclude '("#emacs"))

(defun +erc ()
  (interactive)
  (erc-tls :server "irc.ea.libera.chat"
           :port 6697
           :nick "Jack.L"
           :password +erc-password))

(provide 'init-telega)
