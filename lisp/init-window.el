;;; -*- lexical-binding: t -*-

;;; windmove
(keymap-global-set "M-1" 'windmove-up)
(keymap-global-set "M-2" 'windmove-left)
(keymap-global-set "M-3" 'windmove-right)
(keymap-global-set "M-4" 'windmove-down)

;;; tab-bar
;;
;; Built-in window layout manager
;; NOTE do not bind =tab-bar-switch-to-prev-tab= and
;; =tab-bar-switch-to-next-tab= to =M-[= or =M-]=, it will make emacs have some
;; bug to auto insert characters after you type everytime.
;;
;; See =tab-prefix-map= to custom key bindings for tab-bar, default is =C-x t=.
(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item
              (format " %s  "
                      (nerd-icons-sucicon "nf-custom-emacs"
                                          :face '(:inherit nerd-icons-purple)))
              tab-bar-menu-bar :help "Menu Bar")))

(defun eat/bar-image ()
  (when (and (display-graphic-p) (image-type-available-p 'pbm))
    (propertize
     " " 'display
     (ignore-errors
       (create-image
        ;; 20 for `dirvish-header-line-height'
        (concat (format "P1\n%i %i\n" 2 30) (make-string (* 2 30) ?1) "\n")
        'pbm t :foreground (face-background 'highlight) :ascent 'center)))))

(setq tab-bar-new-tab-choice 'ibuffer
      tab-bar-close-last-tab-choice 'tab-bar-mode-disable
      tab-bar-tab-hints nil
      tab-bar-close-button-show nil
      tab-bar-separator ""
      tab-bar-format '(tab-bar-format-menu-bar
                       tab-bar-format-tabs)
      ;; NOTE 如果要用到很多 tab 导致 tab 换行的话就把这个设置为 t
      tab-bar-auto-width nil
      tab-bar-tab-name-format-function
      (lambda (tab i) "Center, taller, better, stronger xD."
        (let* ((current-tab-p (eq (car tab) 'current-tab))
               (bar (when current-tab-p (eat/bar-image)))
               (name (string-trim (alist-get 'name tab)))
               (space-to-add (max 0 (- tab-bar-tab-name-truncated-max (length name))))
               (left-padding (/ space-to-add 2))
               (right-padding (- space-to-add left-padding)))
          (concat
           bar
           (propertize (concat ;; (propertize " " 'display '(raise 0.3))
                        (make-string left-padding ?\s)
                        name
                        (make-string right-padding ?\s)
                        ;; (propertize " " 'display '(raise -0.3))
                        )
                       'face (funcall tab-bar-tab-face-function tab)))))
      tab-bar-tab-name-function
      (lambda nil "Use project as tab name."
        (let ((dir (expand-file-name
                    (or (if (and (fboundp 'project-root) (project-current))
                            (project-root (project-current)))
                        default-directory))))
          (or
           (and dir
                (let ((name (substring dir (1+ (string-match "/[^/]+/$" dir)) -1)))
                  (truncate-string-to-width name tab-bar-tab-name-truncated-max nil ? )))
           (buffer-name)))))

(with-eval-after-load 'tab-bar
  (tab-bar-history-mode 1))

;;; project-x, save window layout by project
(install-package 'project-x "https://github.com/karthink/project-x")
(with-eval-after-load 'project
  (project-x-mode 1))

;;; popper
(install-package 'popper)

(add-hook 'after-init-hook #'popper-mode)

(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*Compile-Log\\*"
        "\\*Completions\\*"
        "\\*Warnings\\*"

        "^\\*eshell.*\\*$" eshell-mode  ;eshell as a popup
        "^\\*shell.*\\*$"  shell-mode   ;shell as a popup
        "^\\*term.*\\*$"   term-mode    ;term as a popup

        ;; help & message
        help-mode
        compilation-mode

        ghelp-page-mode
        "^\\*eat.*\\*$" eat-mode))

(with-eval-after-load 'project
  (setq popper-group-function 'popper-group-by-project))

(with-eval-after-load 'popper
  (keymap-global-set "C-M-`" #'popper-toggle-type)
  (defun my-popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 3)
     (floor (frame-height) 3)))
  (setq popper-window-height #'my-popper-fit-window-height))

;;; ace-window
(install-package 'ace-window)
(keymap-global-set "M-o" 'ace-window)
(setq aw-keys '(?a ?o ?e ?u ?i))

;;; golden-ratio
(install-package 'golden-ratio)
(define-key mode-specific-map "\\" 'golden-ratio)

(provide 'init-window)
