;;; -*- lexical-binding: t -*-

(eat-package leetcode
  :straight t
  :commands leetcode
  :init
  (setq leetcode-prefer-language "golang"))

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

(provide 'init-mole)
