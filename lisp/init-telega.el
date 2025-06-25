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

;;; init-telega.el ends here
