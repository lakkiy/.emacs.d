;;; -*- lexical-binding: t -*-

(defvar +rime-id "emacs-rime")
(defvar +rime-sync-dir (expand-file-name "~/Dropbox/RimeSync"))


;; You should edit `installation.yam' yourself to change `sync_dir' or `instanllation_id'.
;; TODO clone personal config
;;      download librime
(defun +rime-init ()
  (interactive)
  (unless (file-exists-p (expand-file-name "rimetest" user-emacs-directory))
    (set-process-sentinel
     (start-process "rime" "*rime*" "git" "clone" "--depth=1" "https://github.com/404cn/Rime.git" (expand-file-name "rimetest" user-emacs-directory))
     (lambda (proc _)
       (let ((status (process-exit-status proc)))
         (if (= 0 status)
             (message "Clone rime config.")
           (message "Failed to clone rime config"))))))
  ;; TODO
  ;; should only use in OSX system
  ;; curl -L -O https://github.com/rime/librime/releases/download/1.7.2/rime-1.7.2-osx.zip
  ;; unzip rime-1.7.2-osx.zip -d ~/.config/emacs/librime
  ;; rm -rf rime-1.7.2-osx.zip
  )

(eat-package rime
  :straight t
  :commands toggle-input-method
  :init
  (defun +rime-predicate-org-syntax-punc-p ()
    (when (eq major-mode 'org-mode)
      (member rime--current-input-key '(91 93 42 126))))

  (defun +rime-predicate-md-syntax-punc-p ()
    (when (eq major-mode 'markdown-mode)
      (member rime--current-input-key '(91 93 96))))

  (setq rime-disable-predicates '(meow-normal-mode-p
                                  meow-motion-mode-p
                                  meow-keypad-mode-p
                                  +rime-predicate-org-syntax-punc-p
                                  +rime-predicate-md-syntax-punc-p)
        rime-inline-predicates '(rime-predicate-space-after-cc-p
                                 rime-predicate-current-uppercase-letter-p
                                 +rime-predicate-md-syntax-punc-p)
        rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v")
        rime-inline-ascii-holder ?a
        default-input-method "rime"
        rime-cursor "|"
        rime-show-candidate 'minibuffer
        rime-title " ㄓ ")
  (when sys/macp
    (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory)))
  :config
  (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable))

(provide 'init-rime)
