;;; -*- lexical-binding: t -*-

;; fontify-patch
(install-package 'fontify-patch "https://github.com/whame/fontify-patch")
(add-hook 'gnus-part-display-hook 'fontify-patch-buffer)

;; w3m, read html mail
(install-package 'w3m)
(when (executable-find "w3m")
  (setq mm-text-html-renderer 'w3m))

;; message
;;
;; https://man.sr.ht/lists.sr.ht/etiquette.md#wrap-lines
;; 在邮件中使用 =auto-fill-column= 进行硬折行。
(setq message-kill-buffer-on-exit t
      message-mail-alias-type 'ecomplete
      user-full-name "Liu Bo"
      user-mail-address "liubolovelife@gmail.com"
      message-signature user-full-name
      smtpmail-smtp-user user-mail-address
      send-mail-function #'smtpmail-send-it
      message-send-mail-function #'message-use-send-mail-function
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(defun my/message-mode-setup ()
  (auto-fill-mode)
  (when (executable-find "ltex-ls")
    (eglot-ensure)))
(add-hook 'message-mode-hook #'my/message-mode-setup)


;; gnus
(setq gnus-use-cache t
      gnus-use-scoring nil
      gnus-suppress-duplicates t
      gnus-novice-user nil
      gnus-expert-user t
      gnus-interactive-exit 'quiet
      gnus-inhibit-startup-message t
      gnus-select-method '(nnnil "")
      gnus-secondary-select-methods
      `((nntp "gmane" (nntp-address "news.gmane.io"))
	    (nntp "nntp.lore.kernel.org")
	    (nnimap "Gmail"
		        (nnimap-user "liubolovelife@gmail.com")
		        (nnimap-inbox "INBOX")
		        (nnimap-address "imap.gmail.com")
		        (nnimap-stream ssl)
		        (nnimap-expunge 'never)
		        ;; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
		        ;; press 'E' to expire email
		        (nnmail-expiry-target "nnimap+Gmail:[Gmail]/Trash")
		        (nnmail-expiry-wait 90)))
      ;; Startup functions
      gnus-save-killed-list nil
      gnus-check-new-newsgroups nil
      ;; No other newsreader is used.
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively
      ;; Emacs 28 introduces a unified query lang
      gnus-search-use-parsed-queries t
      ;; Article mode for Gnus
      gnus-visible-headers (rx line-start (or "From"
					                          "Subject"
					                          "Mail-Followup-To"
					                          "Date"
					                          "To"
					                          "Cc"
					                          "Newsgroups"
					                          "User-Agent"
					                          "X-Mailer"
					                          "X-Newsreader")
			                   ":")
      gnus-article-sort-functions '((not gnus-article-sort-by-number)
				                    (not gnus-article-sort-by-date))
      gnus-article-browse-delete-temp t
      ;; Display more MINE stuff
      gnus-mime-display-multipart-related-as-mixed t
      ;; Asynchronous support for Gnus
      gnus-asynchronous t
      gnus-use-header-prefetch t
      ;; Cache interface for Gnus
      gnus-cache-enter-articles '(ticked dormant unread)
      gnus-cache-remove-articles '(read)
      gnus-cacheable-groups "^\\(nntp\\|nnimap\\)")

;; Group
(setq
 ;;          indentation ------------.
 ;;  #      process mark ----------. |
 ;;                level --------. | |
 ;;           subscribed ------. | | |
 ;;  %          new mail ----. | | | |
 ;;  *   marked articles --. | | | | |
 ;;                        | | | | | |  Ticked    New     Unread  open-status Group
 gnus-group-line-format "%M%m%S%L%p%P %1(%7i%) %3(%7U%) %3(%7y%) %4(%B%-45G%) %d\n"
 gnus-group-sort-function '(gnus-group-sort-by-level gnus-group-sort-by-alphabet))

(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)

;; Summary
(setq gnus-sum-thread-tree-root            "┌ "
      gnus-sum-thread-tree-false-root      "◌ "
      gnus-sum-thread-tree-single-indent   "◎ "
      gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-indent          "  "
      gnus-sum-thread-tree-leaf-with-other "├─►"
      gnus-sum-thread-tree-single-leaf     "╰─►"
      gnus-summary-line-format "%U%R %3d %[%-23,23f%] %B %s\n"
      ;; Threads!  I hate reading un-threaded email -- especially mailing
      ;; lists.  This helps a ton!
      gnus-summary-make-false-root 'adopt
      gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace)
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      ;; Filling in threads
      ;; 2 old articles are enough for memory
      gnus-fetch-old-headers 2
      gnus-fetch-old-ephemeral-headers 2
      gnus-build-sparse-threads 'some
      ;; More threading
      gnus-show-threads t
      gnus-thread-indent-level 2
      gnus-thread-hide-subtree nil
      ;; Sorting
      gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
      gnus-subthread-sort-functions '(gnus-thread-sort-by-date)
      ;; Viewing
      gnus-view-pseudos 'automatic
      gnus-view-pseudos-separately t
      gnus-view-pseudo-asynchronously t
      ;; No auto select
      gnus-auto-select-first nil
      gnus-auto-select-next nil
      gnus-paging-select-next nil)

(add-hook 'gnus-select-group-hook #'gnus-group-set-timestamp)
(add-hook 'gnus-summary-mode-hook #'hl-line-mode)

;;; init-mail.el ends here
(provide 'init-mail)
