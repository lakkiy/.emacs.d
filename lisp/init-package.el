;;; -*- lexical-binding: t -*-

(setq package-archives
      '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; To prevent initializing twice
(setq package-enable-at-startup nil)
(package-initialize)

(defun install-package (pkg &optional url)
  (unless (package-installed-p pkg)
    (if url
        (package-vc-install url)
      (unless (assoc pkg package-archive-contents)
        (package-refresh-contents))
      (package-install pkg))))

(provide 'init-package)
