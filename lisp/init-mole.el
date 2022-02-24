;;; -*- lexical-binding: t -*-

;; `newsticker'
(setq newsticker-frontend 'newsticker-plainview)

;; `gnus'
(setq
 gnus-use-cache t
 gnus-use-scoring nil
 gnus-suppress-duplicates t
 gnus-novice-user nil
 gnus-expert-user t
 gnus-interactive-exit 'quiet
 gnus-select-method '(nnnil "")
 gnus-secondary-select-methods '((nntp "gmane" (nntp-address "news.gmane.io"))
                                 (nntp "nntp.lore.kernel.org"))
    ;;; Startup functions
 gnus-save-killed-list nil
 gnus-check-new-newsgroups 'ask-server
 ;; No other newsreader is used.
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil
 gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively
 ;; Emacs 28 introduces a unified query lang
 gnus-search-use-parsed-queries t
    ;;; Article mode for Gnus
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
  ;;; Asynchronous support for Gnus
 gnus-asynchronous t
 gnus-use-header-prefetch t
  ;;; Cache interface for Gnus
 gnus-cache-enter-articles '(ticked dormant unread)
 gnus-cache-remove-articles '(read)
 gnus-cacheable-groups "^\\(nntp\\|nnimap\\)")


;; `gnus-group'
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

;; `gnus-sum'
(setq
 ;; Pretty marks
 gnus-sum-thread-tree-root            "┌ "
 gnus-sum-thread-tree-false-root      "◌ "
 gnus-sum-thread-tree-single-indent   "◎ "
 gnus-sum-thread-tree-vertical        "│"
 gnus-sum-thread-tree-indent          "  "
 gnus-sum-thread-tree-leaf-with-other "├─►"
 gnus-sum-thread-tree-single-leaf     "╰─►"
 gnus-summary-line-format "%U%R %3d %[%-23,23f%] %B %s\n"
 ;; Loose threads
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
 gnus-auto-select-next nil)

(add-hook 'gnus-select-group-hook #'gnus-group-set-timestamp)

(eat-package elfeed
  :straight t
  :commands elfeed
  :init
  (global-set-key (kbd "C-x w") 'elfeed)

  (setq elfeed-search-filter "+unread "
        elfeed-curl-extra-arguments '("--insecure")
        ;; browse-url-browser-function 'eww-browse-url
        ;; HACK only for emacs 29
        browse-url-browser-function 'xwidget-webkit-browse-url)

  (defun +elfeed-search-star-tag-all ()
    (interactive)
    (elfeed-search-tag-all 'star))

  (defun +elfeed-search-star-untag-all ()
    (interactive)
    (elfeed-search-untag-all 'star))
  :hook (eww-mode-hook . visual-line-mode)
  :config
  (define-key elfeed-search-mode-map (kbd "t") '+elfeed-search-star-tag-all)
  (define-key elfeed-search-mode-map (kbd "T") '+elfeed-search-star-untag-all)
  (elfeed-set-timeout 36000))

(eat-package elfeed-protocol
  :straight t
  :after elfeed
  :config
  (elfeed-protocol-enable))

;; secret mode
(straight-use-package 'redacted)

(provide 'init-mole)
