;;; init-text.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; image
(autoload #'iimg-enable "iimg")
(add-hook 'text-mode-hook #'iimg-enable)
(setq iimg-prune-slices-p nil)

;; TODO pngpaste and other system
;; TODO run screenshot and paste
(defun iimg-insert-clipboard (name)
  (interactive
   (list (let ((name (read-string "Caption/name for the image: ")))
           (if (equal name "")
               (format-time-string "%s")
             name))))
  (let ((image-file "/tmp/iimg.png"))
    (if (zerop (shell-command (concat "wl-paste -t image/png > " image-file)))
        (progn
          (iimg-insert image-file name t)
          (message "Image inserted successfully: %s" name)
          (delete-file image-file))
      (message "Failed to paste image from clipboard."))))

;;; markdown
(install-package 'markdown-mode)
(install-package 'markdown-indent-mode)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook #'markdown-indent-mode)

(defun lakki.is/reading-markdown ()
  (when (executable-find "ltex-ls")
    (eglot-ensure))
  (when (display-graphic-p)
    (hide-mode-line-mode 1)

    ;; ── 换行策略：软换行（视觉换行，不插入硬换行符）──
    (visual-line-mode 1)                ; 在窗口边缘视觉换行
    (auto-fill-mode -1)                 ; 关闭硬换行（不自动插入 \n）


    ;; ── 阅读宽度：限制在 88 字符，内容居中 ──
    (setq-local visual-fill-column-width 88
                visual-fill-column-center-text t)
    (visual-fill-column-mode 1)

    ;; ── 字体大小：标题按层级放大 ──
    (setq-local markdown-header-scaling t
                markdown-header-scaling-values '(2.0 1.7 1.4 1.1 1.0 1.0))

    ;; ── 表格对齐 ──
    (valign-mode 1)

    ;; ── 正文用比例字体，代码块用等宽字体 ──
    (mixed-pitch-mode 1)

    ;; ── 隐藏标记符号，获得更干净的视觉效果 ──
    (setq-local markdown-hide-markup t) ; 改为 t 可隐藏 * _ 等符号
    (setq-local markdown-hide-urls t)   ; 隐藏链接 URL，只显示文字

    ;; ── 代码块语法高亮 ──
    (setq-local markdown-fontify-code-blocks-natively t)

    ;; ── 行间距，提升可读性 ──
    (setq-local line-spacing 0.15)

    ;; ── 光标不进入折叠区域 ──
    (setq-local markdown-cycle-atx-headers t)))
(add-hook 'markdown-mode-hook 'lakki.is/reading-markdown)

;;; LaTeX
(defun my/latex-mode-setup ()
  (when (executable-find "digestif")
    (company-mode 1)
    (eglot-ensure)))
(add-hook 'latex-mode-hook 'my/latex-mode-setup)

;;; typst
(install-package 'typst-ts-mode "https://git.sr.ht/~meow_king/typst-ts-mode")

;;; init-text.el ends here
