;; -*- lexical-binding: t; -*-

(straight-use-package '(telega :type git :host github :branch "releases"))

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
 telega-old-date-format "%Y/%M/%D")

(autoload #'telega "telega" nil t)

;; set char width for certain characters
(defun blaenk/set-char-widths (alist)
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (dolist (pair alist)
    (let ((width (car pair))
          (chars (cdr pair))
          (table (make-char-table nil)))
      (dolist (char chars)
        (set-char-table-range table char width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

;; pgtk emacs does not support copying image from clipboard, a simple workaround
(define-advice telega-chatbuf-attach-clipboard (:override (doc-p) yang)
  "Attach clipboard image to the chatbuf as photo.
If `\\[universal-argument]' is given, then attach clipboard as document."
  (interactive "P")
  (let* ((selection-coding-system 'no-conversion) ;for rawdata
         (temporary-file-directory telega-temp-dir)
         (tmpfile (telega-temp-name "clipboard" ".png"))
         (coding-system-for-write 'binary))
    (if (eq system-type 'gnu/linux)
        (shell-command-to-string
         (format "xclip -selection clipboard -t image/png -o > %s"
                 tmpfile))
      (write-region (or (gui-get-selection 'CLIPBOARD 'image/png)
                        (error "No image in CLIPBOARD"))
                    nil tmpfile nil 'quiet))
    (telega-chatbuf--attach-tmp-photo tmpfile doc-p)))

(defun yang/fixed-pitch-setup ()
  (interactive)
  (setq buffer-face-mode-face '(:family "等距更纱黑体 SC"))
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
  (add-hook 'telega-chat-mode (lambda ()
                                (set-company-backend! 'telega-chat-mode
                                                      (append '(telega-company-emoji
                                                                telega-company-username
                                                                telega-company-hashtag)
                                                              (when (telega-chat-bot-p telega-chatbuf--chat)
                                                                '(telega-company-botcmd))))))

  (blaenk/set-char-widths
   '((2 . (?— ?… ?🅥 ?⌕ ))
     (3 . (?㊙ ?💉 ?🌊 ?🇨 ?🇳 ?🚴 ?🚗 ?📖
               ?🏎 ))))
  ;; syntax highlighting in telega code
  (require 'telega-mnz)
  (global-telega-mnz-mode 1))

(provide 'init-telega)
