;; -*- lexical-binding: t; -*-

;; emacs as app launcher
(eat-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher")
  :init
  (defun eat/emacs-app-launcher ()
    "Create and select a frame called emacs-app-launcher which consists only of a minibuffer and has specific dimensions.
Run `app-launcher-run-app' on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour.
Delete the frame after that command has exited"
    (interactive)
    (let ((default-frame-alist '((undecorated . t)
                                 (vertical-scroll-bars)
                                 (scroll-bar-mode . 0)
                                 (menu-bar-lines . 0)
                                 (tool-bar-lines . 0))))
      (with-selected-frame
          (make-frame
           '((name . "emacs-app-launcher")
             (minibuffer . only)
             (width . 120)
             (height . 11)))
        (unwind-protect
            (app-launcher-run-app)
          (delete-frame))))))

(eat-package telega
  :straight t
  :commands telega
  :hook
  (telega-load-hook . (telega-notifications-mode telega-appindicator-mode))
  :config
  ;; ignore blocked user
  (add-hook 'telega-msg-ignore-predicates
            (telega-match-gen-predicate "msg-" '(sender blocked)))

  (setq telega-chat-input-prompt "> "
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

(eat-package magit
  :straight t
  :hook
  (git-commit-setup-hook . git-commit-turn-on-flyspell)
  (magit-diff-visit-file . my-recenter-and-pulse-line)
  :commands magit
  :init
  (defun eat/magit-yadm ()
    (interactive)
    (magit-status "/yadm::"))
  :config
  (fullframe magit-status magit-mode-quit-window)
  (setq-default magit-diff-refine-hunk t)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch))

(when (executable-find "delta")
  (eat-package magit-delta
    :straight t
    :hook (magit-mode-hook . magit-delta-mode)))

(eat-package diff-hl
  :straight t
  :commands diff-hl-mode
  :hook
  ((prog-mode-hook conf-mode-hook) . diff-hl-mode)
  (dired-mode-hook . diff-hl-dired-mode)
  (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :init
  (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil)))))

(eat-package dirvish
  :straight '(dirvish :files (:defaults "extensions/*"))
  :hook (after-init-hook . dirvish-override-dired-mode)
  :init
  (setq dirvish-attributes '(vc-state subtree-state all-the-icons))
  (global-set-key (kbd "<f1>") #'dirvish-side)
  (global-set-key (kbd "C-c f") #'dirvish-fd)
  :config
  (define-key dirvish-mode-map (kbd "TAB") #'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "<tab>") #'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "a") #'dirvish-quick-access)
  (define-key dirvish-mode-map (kbd "f") #'dirvish-file-info-menu)
  (define-key dirvish-mode-map (kbd "y") #'dirvish-yank-menu)
  (define-key dirvish-mode-map (kbd "N") #'dirvish-narrow)
  (define-key dirvish-mode-map (kbd "H") #'dirvish-history-jump) ; remapped `describe-mode'
  (define-key dirvish-mode-map (kbd "s") #'dirvish-quicksort) ; remapped `dired-sort-toggle-or-edit'
  (define-key dirvish-mode-map (kbd "v") #'dirvish-vc-menu)  ; remapped `dired-view-file'
  (define-key dirvish-mode-map (kbd "M-f") #'dirvish-history-go-forward)
  (define-key dirvish-mode-map (kbd "M-b") #'dirvish-history-go-backward)
  (define-key dirvish-mode-map (kbd "M-l") #'dirvish-ls-switches-menu)
  (define-key dirvish-mode-map (kbd "M-m") #'dirvish-mark-menu)
  (define-key dirvish-mode-map (kbd "M-t") #'dirvish-layout-toggle)
  (define-key dirvish-mode-map (kbd "M-s") #'dirvish-setup-menu)
  (define-key dirvish-mode-map (kbd "M-e") #'dirvish-emerge-menu)
  (define-key dirvish-mode-map (kbd "M-j") #'dirvish-fd-jump)
  ;; mouse
  ;; (setq mouse-1-click-follows-link nil)
  (define-key dirvish-mode-map (kbd "<mouse-1>") #'dirvish-subtree-toggle-or-open)
  (define-key dirvish-mode-map (kbd "<mouse-2>") #'dired-mouse-find-file-other-window)
  (define-key dirvish-mode-map (kbd "<mouse-3>") #'dired-mouse-find-file))

(defface eat/notmuch-tag-emacs
  '((t :foreground "systemPurpleColor"))
  "Default face used for the Emacs tag.

Used in the default value of `notmuch-tag-formats'."
  :group 'notmuch-faces)

(defface eat/notmuch-tag-golang
  '((t :foreground "systemBlueColor"))
  "Default face used for the golang tag.

Used in the default value of `notmuch-tag-formats'."
  :group 'notmuch-faces)

(eat-package notmuch
  :straight t
  :commands notmuch
  :init
  (setq notmuch-search-oldest-first nil
        notmuch-search-result-format '(("date" . "%12s ")
                                       ("count" . "%-11s ")
                                       ("authors" . "%-20s ")
                                       ("subject" . "%-80s ")
                                       ("tags" . "(%s)"))
        notmuch-show-empty-searches t)
  (defun eat/async-notmuch-poll ()
    (interactive)
    (message "Start polling email...")
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`load-path\\'")
        (require 'notmuch)
        (notmuch-poll))
     (lambda (result)
       (message "eat/async-notmuch-poll: %s" result)
       (notify-send :title "Emacs" :body result :urgency 'critical))))
  :config
  (add-to-list 'notmuch-tag-formats '("emacs" (propertize tag 'face 'eat/notmuch-tag-emacs)))
  (add-to-list 'notmuch-tag-formats '("golang" (propertize tag 'face 'eat/notmuch-tag-golang)))
  (global-set-key [remap notmuch-poll-and-refresh-this-buffer] #'eat/async-notmuch-poll))

(eat-package docker
  :straight t
  :commands docker
  :config
  (fullframe docker-images tablist-quit)
  (fullframe docker-machines tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit))

(eat-package kubernetes
  :straight t
  :commands
  kubernetes-overview
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(eat-package devdocs :straight t)

(eat-package ibuffer-vc
  :straight t
  :hook (ibuffer-hook . ibuffer-set-up-preferred-filters)
  :init
  (defun ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process))))

(eat-package vterm
  :straight t
  :init
  (setq vterm-always-compile-module t)

  (eat-package vterm-toggle
    :straight t
    :init
    (global-set-key (kbd "C-`") #'vterm-toggle))

  (eat-package multi-vterm
    :straight t
    :init
    (global-set-key (kbd "C-~") #'multi-vterm)
    :config
    (define-key vterm-mode-map (kbd "C-(") #'multi-vterm-prev)
    (define-key vterm-mode-map (kbd "C-)") #'multi-vterm-next)))

;;; init-app.el ends here
(provide 'init-app)
