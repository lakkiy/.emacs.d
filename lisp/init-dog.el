;;; -*- lexical-binding: t -*-

;; curl -L -O https://github.com/rime/librime/releases/download/1.7.2/rime-1.7.2-osx.zip
;; unzip rime-1.7.2-osx.zip -d ~/.config/emacs/librime
;; rm -rf rime-1.7.2-osx.zip
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
        rime-show-candidate 'minibuffer)
  (when sys/macp
    (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory)))
  :config
  (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable))

;; TODO remoce `dash.el'
(defun +project-previous-buffer (arg)
  "Toggle to the previous buffer that belongs to current project."
  (interactive "P")
  (if (equal '(4) arg)
      (if-let ((pr (project-current)))
          (switch-to-buffer
           (->> (project--buffer-list pr)
                (--remove (or (minibufferp it)
                              (get-buffer-window-list it)))
                (car))))
    (mode-line-other-buffer)))

(eat-package meow
  :straight t
  :hook
  (after-init-hook . (lambda ()
                       (meow-global-mode 1)))
  :config
  ;; custom indicator
  (setq meow-replace-state-name-list
        '((normal . "🅝")
          (beacon . "🅑")
          (insert . "🅘")
          (motion . "🅜")
          (keypad . "🅚")))

  (meow-setup-indicator) ;; NOTE use default modeline

  ;; SPC h f to describe-funtion
  (global-set-key (kbd "C-h C-f") 'describe-funtion)

  ;; normal mode list
  (dolist (mode '(go-dot-mod-mode
                  diff-mode))
    (add-to-list 'meow-mode-state-list `(,mode . normal)))
  ;; motion mode list
  (dolist (mode '(notmuch-hello-mode
                  notmuch-search-mode
                  notmuch-tree-mode))
    (add-to-list 'meow-mode-state-list `(,mode . motion)))

  ;; setup meow with selected keyboard layout
  (cond ((eq +meow-layout 'dvorak)
         (require 'init-meow-dvorak)
         (meow-setup-dvorak))
        (t
         (require 'init-meow-qwerty)
         (meow-setup-qwerty)))
  :init
  (setq meow-esc-delay 0.001))

(eat-package window-numbering
  :straight (window-numbering :type git :host github :repo "DogLooksGood/window-numbering.el")
  :hook (after-init-hook . (lambda ()
                             (require 'window-numbering)
                             (window-numbering-mode 1))))

(provide 'init-dog)
