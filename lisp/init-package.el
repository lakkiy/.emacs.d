;;; -*- lexical-binding: t -*-

(setq package-archives
      '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; To prevent initializing twice
(setq package-enable-at-startup nil)
;; gptel need newest org mode
(setq package-install-upgrade-built-in t)
(package-initialize)

(defun install-package (pkg &optional url)
  (unless (package-installed-p pkg)
    (if url
        (package-vc-install url)
      (unless (package-installed-p pkg)
        (package-refresh-contents))
      (package-install pkg))))
(install-package 'sicp)

;;; init-package.el ends here
