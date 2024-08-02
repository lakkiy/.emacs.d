;;; -*- lexical-binding: t -*-
;;
;; If install emacs with nix:
;; Set =rime-emacs-module-header-root= to =emacs/include=.
;; set to =librime=.
(install-package 'rime)

;; TODO auto 从 librime 仓库下载相应的预编译包
;; TODO change emacs rime install id and sync dir

(if (eq system-type 'darwin)
    (progn
      ;; a folder contain emacs-module.h
      (setq rime-emacs-module-header-root
            (concat
             (file-name-directory
              (directory-file-name
               (file-name-directory
                (directory-file-name
                 (file-name-directory
                  (directory-file-name
                   (file-name-directory invocation-directory)))))))
             "include"))
      (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory)))
  (setq rime-share-data-dir "~/.local/share/fcitx5/rime"))

(setq rime-disable-predicates '(meow-normal-mode-p
				                meow-motion-mode-p
				                meow-keypad-mode-p
				                meow-beacon-mode-p)
      rime-inline-predicates '(rime-predicate-space-after-cc-p
                               rime-predicate-current-uppercase-letter-p)
      rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v")
      rime-inline-ascii-holder ?a
      default-input-method "rime"
      rime-cursor "|"
      rime-show-candidate 'minibuffer)

(with-eval-after-load 'rime
  (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
  (keymap-set rime-mode-map "M-j" 'rime-force-enable))

;;; init-rime.el ends here
(provide 'init-rime)
