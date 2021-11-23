;;; -*- lexical-binding: t -*-

(eat-package elfeed
  :straight t
  :commands elfeed
  :init
  (global-set-key (kbd "C-x w") 'elfeed)

  (setq elfeed-search-filter "+unread "
        elfeed-curl-extra-arguments '("--insecure")
        browse-url-browser-function 'eww-browse-url)

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

(eat-package declutter
  :straight (declutter :type git :host github :repo "sanel/declutter")
  :commands declutter-under-point
  :init
  (setq declutter-engine 'rdrview))

(eat-package leetcode
  :straight t
  :init
  (setq leetcode-prefer-language "golang")
  (setq leetcode-prefer-sql "mysql")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/Dropbox/leetcode"))

(provide 'init-mole)
