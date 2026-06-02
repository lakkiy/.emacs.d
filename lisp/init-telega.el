;;; init-telega.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(install-package 'telega)

(setq telega-chat-fill-column fill-column
      ;; 头像再也不裂了
      telega-avatar-workaround-gaps-for '(return t)
      ;; 默认翻译目标语言为中文
      telega-translate-to-language-by-default "zh"
      ;; send code in markdown format
      telega-chat-input-markups '("markdown2" "org")
      ;; 使用 capf 代替 telega 默认的 ido 补全
      telega-completing-read-function completing-read-function
      ;; 省略消息中过长的 url
      telega-url-shorten-regexps (list `(too-long-link
					                     :regexp "^\\(https?://\\)\\(.\\{55\\}\\).*?$"
					                     :symbol ""
					                     :replace "\\1\\2...")))

(add-hook 'telega-root-mode-hook 'hl-line-mode)
(add-hook 'telega-chat-mode-hook 'company-mode)

(with-eval-after-load 'telega
  (add-hook 'telega-load-hook #'telega-notifications-mode)
  (add-hook 'telega-load-hook #'telega-appindicator-mode)

  (setq telega-chat-input-format "›"
	    telega-animation-play-inline nil
	    telega-video-play-inline nil
	    ;; make sticker larger to read
	    telega-sticker-size '(10 . 24)
	    ;; change reply symbol
	    telega-symbol-reply "↫"
	    ;; set date format for old messages
	    telega-old-date-format "%Y/%M/%D")

  ;; syntax highlighting in telega code
  (require 'telega-mnz)
  (global-telega-mnz-mode 1))

;; Dired-like marking for deleting spam private chats from the root buffer.
(with-eval-after-load 'telega
  ;; Let telega's mode-local keys win over Meow's normal-state bindings.
  (with-eval-after-load 'meow
    (add-to-list 'meow-mode-state-list '(telega-root-mode . motion)))

  (defvar my-telega-marked-chat-ids nil
    "Chat IDs marked for bulk deletion in the telega root buffer.")

  (defun my-telega-chat-marked-p (chat)
    "Return non-nil when CHAT is marked for bulk deletion."
    (member (plist-get chat :id) my-telega-marked-chat-ids))

  (defun my-telega-ins-chat-mark (chat)
    "Insert a mark in front of CHAT when it is marked for deletion."
    (when (my-telega-chat-marked-p chat)
      (telega-ins (propertize "* " 'face 'error))))

  (defun my-telega-chat-at-point ()
    "Return the telega chat at point, or signal a user error."
    (or (telega-chat-at (point))
        (user-error "No telega chat at point")))

  (defun my-telega-next-chat ()
    "Move point to the next chat button, if one exists."
    (telega-button-forward
        1
      (lambda (button)
        (eq (button-type button) 'telega-chat))
      t))

  (defun my-telega-mark-chat ()
    "Mark the private chat at point for bulk deletion."
    (interactive)
    (let ((chat (my-telega-chat-at-point)))
      (unless (telega-chat-private-p chat)
        (user-error "Only private and bot chats can be marked"))
      (cl-pushnew (plist-get chat :id) my-telega-marked-chat-ids
                  :test #'equal)
      (telega-root-view--update :on-chat-update chat)
      (my-telega-next-chat)))

  (defun my-telega-unmark-chat ()
    "Remove the deletion mark from the chat at point."
    (interactive)
    (let ((chat (my-telega-chat-at-point)))
      (setq my-telega-marked-chat-ids
            (delete (plist-get chat :id) my-telega-marked-chat-ids))
      (telega-root-view--update :on-chat-update chat)
      (my-telega-next-chat)))

  (defun my-telega-delete-private-chat (chat)
    "Block the sender and delete private CHAT without prompting."
    (unless (telega-chat-private-p chat)
      (error "Refusing to delete non-private chat: %s"
             (telega-chat-title chat)))
    (unless (telega-chat-match-p
             chat
             '(or (prop :can_be_deleted_only_for_self)
                  (prop :can_be_deleted_for_all_users)))
      (error "Chat cannot be deleted: %s" (telega-chat-title chat)))

    (unless (telega-chat-match-p chat '(is-blocked blockListMain))
      (telega-msg-sender-block chat))
    (setq telega-deleted-chats
          (cl-pushnew chat telega-deleted-chats))
    ;; Delete only for this account; do not revoke history for the sender.
    (telega--deleteChatHistory chat 'remove-from-list)
    (with-telega-chatbuf chat
      (kill-buffer (current-buffer))))

  (defun my-telega-delete-marked-chats ()
    "Block and delete all marked private chats after one confirmation."
    (interactive)
    (let ((chats (delq nil
                       (mapcar (lambda (chat-id)
                                 (telega-chat-get chat-id 'offline))
                               my-telega-marked-chat-ids))))
      (unless chats
        (setq my-telega-marked-chat-ids nil)
        (user-error "No telega chats are marked"))
      (when (yes-or-no-p
             (format "Block senders and delete %d marked chat%s? "
                     (length chats)
                     (if (= (length chats) 1) "" "s")))
        (let (failed-chat-ids)
          (dolist (chat chats)
            (condition-case err
                (my-telega-delete-private-chat chat)
              (error
               (push (plist-get chat :id) failed-chat-ids)
               (message "Failed to delete %s: %s"
                        (telega-chat-title chat)
                        (error-message-string err)))))
          (setq my-telega-marked-chat-ids (nreverse failed-chat-ids))
          (telega-root-view--redisplay)
          (if failed-chat-ids
              (message "Bulk deletion submitted; %d chat(s) failed"
                       (length failed-chat-ids))
            (message "Bulk deletion submitted for %d chat(s)"
                     (length chats)))))))

  (unless (advice-member-p #'my-telega-ins-chat-mark 'telega-ins--chat)
    (advice-add 'telega-ins--chat :before #'my-telega-ins-chat-mark))

  ;; Bind both maps because point is normally inside a chat text button.
  (dolist (map (list telega-root-mode-map telega-chat-button-map))
    (keymap-set map "m" #'my-telega-mark-chat)
    (keymap-set map "u" #'my-telega-unmark-chat)
    (keymap-set map "x" #'my-telega-delete-marked-chats)))

;;; init-telega.el ends here
