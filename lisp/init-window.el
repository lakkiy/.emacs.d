;;; init-window.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; popper
(install-package 'popper)
(add-hook 'after-init-hook #'popper-mode)

(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*Compile-Log\\*"
        "\\*Completions\\*"
        "\\Agenda Commands\\*"
        help-mode
        compilation-mode
        ghelp-page-mode

        "^\\*eshell.*\\*$" eshell-mode  ;eshell as a popup
        "^\\*shell.*\\*$"  shell-mode   ;shell as a popup
        "^\\*term.*\\*$"   term-mode    ;term as a popup
        "^\\*eat.*\\*$" ;; eat-mode popper eat buffer but not claude-code
        ))

(global-set-key (kbd "C-`") 'popper-toggle)
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)

(with-eval-after-load 'popper
  (require 'popper-echo)
  (popper-echo-mode +1)

  (defun lakki.is/popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 3)
     (floor (frame-height) 3)))
  (setq popper-window-height #'lakki.is/popper-fit-window-height))

;;; Tab bar
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

(defun my/bar-image ()
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
               (bar (when current-tab-p (my/bar-image)))
               (name (string-trim (alist-get 'name tab)))
               (space-to-add (max 0 (- tab-bar-tab-name-truncated-max (length name))))
               (left-padding (/ space-to-add 2))
               (right-padding (- space-to-add left-padding)))
          (concat
           ;; bar
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

;; https://www.emacs.dyerdwelling.family/emacs/20240817082349-emacs--syncing-tab-bar-to-theme/
(defun my/sync-tab-bar-to-theme ()
  "Synchronize tab-bar faces with the current theme."
  (interactive)
  (let ((default-bg (face-background 'default))
        (default-fg (face-foreground 'default))
        (inactive-fg (face-foreground 'mode-line-inactive)))
    (custom-set-faces
     `(tab-bar ((t (:inherit default :background ,default-bg :foreground ,default-fg))))
     `(tab-bar-tab ((t (:inherit default :background ,default-fg :foreground ,default-bg))))
     `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg)))))))
(add-hook 'after-load-theme-hook #'my/sync-tab-bar-to-theme)

;;; Window
;;
;; The native border "uses" a pixel of the fringe on the rightmost
;; splits, whereas `window-divider-mode' does not.
(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

;; Undo window change
(setq winner-dont-bind-my-keys t)
(add-hook 'after-init-hook #'winner-mode)

(keymap-global-set "M-o" 'other-window)

;; Skip window by setting no-other-window window parameter in
;; display-buffer-alist for specific buffer(like dired-sidebar).

;; When splitting window, show (other-buffer) in the new window
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window with other-buffer unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (unless arg
        (set-window-buffer target-window (other-buffer))
        (select-window target-window)))))

(keymap-global-set "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically))
(keymap-global-set "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally))

;;; init-window.el ends here
