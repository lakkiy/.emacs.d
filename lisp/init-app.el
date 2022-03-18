;; -*- lexical-binding: t; -*-

(eat-package telega
  :straight t
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

  ;; enable some completion in telega chatbuf
  (setq telega-emoji-company-backend 'telega-company-emoji)

  (defun my-telega-chat-mode ()
    (set (make-local-variable 'company-backends)
         (append (list telega-emoji-company-backend
                       'telega-company-username
                       'telega-company-hashtag)
                 (when (telega-chat-bot-p telega-chatbuf--chat)
                   '(telega-company-botcmd))))
    (company-mode 1))

  (add-hook 'telega-chat-mode-hook 'my-telega-chat-mode)

  ;; syntax highlighting in telega code
  (require 'telega-mnz)
  (global-telega-mnz-mode 1))

(eat-package magit
  :straight t
  :hook (git-commit-setup-hook . git-commit-turn-on-flyspell)
  :commands magit)

(when (executable-find "delta")
  (eat-package magit-delta
    :straight t
    :init
    (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))))

(eat-package diff-hl
  :straight t
  :commands diff-hl-mode
  :hook
  ((prog-mode-hook conf-mode-hook) . diff-hl-mode)
  (dired-mode-hook . diff-hl-dired-mode)
  :init
  (setq diff-hl-draw-borders nil)
  :config
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode))
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil)))))

(eat-package dirvish
  :straight t
  :config
  (dirvish-override-dired-mode))

(eat-package message
  :hook (message-mode-hook . auto-fill-mode)
  :init
  (setq
   user-full-name "Liu Bo"
   user-mail-address "liubolovelife@gmail.com"
   message-kill-buffer-on-exit t
   message-mail-alias-type 'ecomplete
   message-send-mail-function #'message-use-send-mail-function
   message-signature user-full-name))

(eat-package sendmail
  :init
  (setq send-mail-function #'smtpmail-send-it))

(eat-package smtpmail
  :init
  (setq
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-user user-mail-address
   smtpmail-smtp-service 587
   smptmail-stream-type 'ssl))

(defface notmuch-tag-emacs
  '((t :foreground "systemPurpleColor"))
  "Default face used for the emacs tag.

Used in the default value of `notmuch-tag-formats'."
  :group 'notmuch-faces)

(defface notmuch-tag-golang
  '((t :foreground "systemBlueColor"))
  "Default face used for the golang tag.

Used in the default value of `notmuch-tag-formats'."
  :group 'notmuch-faces)

(eat-package notmuch
  :straight t
  :commands notmuch
  :init
  (setq notmuch-search-oldest-first nil
        notmuch-search-result-format '(("date" . "%12s ")
                                       ("count" . "%-11s ")
                                       ("authors" . "%-20s ")
                                       ("subject" . "%-40s ")
                                       ("tags" . "(%s)"))
        notmuch-show-empty-searches t)
  (defun +async-notmuch-poll ()
    (interactive)
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`load-path\\'")
        (require 'notmuch)
        (notmuch-poll))
     (lambda (result)
       (message "+async-notmuch-poll: %s" result)
       (notify-send :title "Emacs" :body result :urgency 'critical))))
  :config
  (add-to-list 'notmuch-tag-formats '("emacs" (propertize tag 'face 'notmuch-tag-emacs)))
  (add-to-list 'notmuch-tag-formats '("golang" (propertize tag 'face 'notmuch-tag-golang)))
  (global-set-key [remap notmuch-poll-and-refresh-this-buffer] #'+async-notmuch-poll))

(eat-package docker
  :straight t
  :commands docker)

(eat-package kubernetes
  :straight t
  :commands
  kubernetes-overview
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(eat-package devdocs :straight t)

(eat-package pulsar
  :straight (pulsar :type git :host github :repo "protesilaos/pulsar")
  :hook (after-init-hook . pulsar-setup)
  :config
  (customize-set-variable
   'pulsar-pulse-functions ; Read the doc string for why not `setq'
   '(recenter-top-bottom
     move-to-window-line-top-bottom
     reposition-window
     bookmark-jump
     other-window
     forward-page
     backward-page
     scroll-up-command
     scroll-down-command
     org-next-visible-heading
     org-previous-visible-heading
     org-forward-heading-same-level
     org-backward-heading-same-level
     org-tree-slide-move-next-tree
     org-tree-slide-move-previous-tree
     outline-backward-same-level
     outline-forward-same-level
     outline-next-visible-heading
     outline-previous-visible-heading
     outline-up-heading))

  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-delay 0.055))

;; TODO only enable this in comment?
(eat-package svg-tag-mode
  :straight (svg-tag-mode :type git :host github :repo "rougier/svg-tag-mode")
  :commands svg-tag-mode
  ;; :hook
  ;; ((prog-mode-hook org-mode-hook) . (lambda ()
  ;;                                     (let ((inhibit-message t))
  ;;                                       (svg-tag-mode))))
  :config
  (setq svg-tag-tags
        '(("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
          ("FIXME" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0))))
          ("\\/\\/\\W?MARK:\\|MARK:" . ((lambda (tag) (svg-tag-make "MARK" :face 'font-lock-doc-face :inverse t :margin 0 :crop-right t))))
          ("MARK:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'font-lock-doc-face :crop-left t))))
          ;; TODOS
          ("\\/\\/\\W?TODO\\|TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0 :crop-right t))))
          ("TODO\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :crop-left t)))))))

(eat-package ibuffer-vc
  :straight t
  :hook (ibuffer-hook . ibuffer-set-up-preferred-filters)
  :init
  (defun ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process))))

(provide 'init-app)
