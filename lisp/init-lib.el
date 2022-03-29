;; -*- lexical-binding: t; -*-

(eat-package all-the-icons :straight t)
(eat-package async :straight t)

;; 拼音搜索
(eat-package pinyinlib
  :straight t
  :commands
  pinyinlib-build-regexp-char
  pinyinlib-build-regexp-string)

(provide 'init-lib)
