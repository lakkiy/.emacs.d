;;; init-chinese.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(install-package 'rime)

(setq rime-disable-predicates '(meow-normal-mode-p
				                meow-motion-mode-p
				                meow-keypad-mode-p
				                meow-beacon-mode-p)
      rime-inline-predicates '(rime-predicate-space-after-cc-p
                               rime-predicate-current-uppercase-letter-p)
      rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v")
      rime-inline-ascii-holder ?a
      default-input-method "rime")

(with-eval-after-load 'rime
  (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
  (keymap-set rime-mode-map "M-j" 'rime-force-enable)
  ;; FIX Emacs quit unexpectedly.
  (add-hook 'kill-emacs-hook #'rime-lib-finalize))

;;; fanyi —— 独立的英文词典，M-x fanyi-dwim2
(install-package 'fanyi)

;;; org 中文行内格式化
;;
;; 例如：
;; org-mode 中/斜体/没效果，必须要在前后都加个空格才行，但中文与中文之间加空格是不可以接受的。
;; 最开始的帖子
;; https://emacs-china.org/t/org-mode/597
;; 太 hack 了，经常碰到不适用的情况
;; https://emacs-china.org/t/org-mode/22313
;; 最新的帖子
;; https://emacs-china.org/t/org-mode/26643
;;
;; 其他中文相关问题也会写在这里
;; https://emacs-china.org/t/org-mode-html/7174
(defun my/insert-zero-width-space ()
  (interactive)
  (insert-char ?\u200B))

;; 导出时（导出为 org 时除外），去除零宽空格
(defun my/org-export-remove-zero-width-space (text _backend _info)
  "Remove zero width spaces from TEXT."
  (unless (org-export-derived-backend-p 'org)
    (replace-regexp-in-string "\u200b" "" text)))
(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-final-output-functions #'my/org-export-remove-zero-width-space t))

;; 使用 prettify 显示零宽空格
;; https://github.com/shynur/.emacs.d/blob/c08a83be390cb44f7cbaa0c01bae2dcd77dbaee3/lisp/shynur-lang.el#L35C46-L35C48
(defun my/display-zero-space ()
  (setq-local prettify-symbols-alist (push '("\u200b" . ?‸) prettify-symbols-alist))
  (prettify-symbols-mode 1))
(add-hook 'org-mode-hook #'my/display-zero-space)

(with-eval-after-load 'org
  (keymap-set org-mode-map "M-SPC" #'my/insert-zero-width-space))

;;; init-chinese.el ends here
