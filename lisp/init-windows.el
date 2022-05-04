;;; -*- lexical-binding: t -*-

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;; replace `tab-bar'
(eat-package eyebrowse
  :straight t
  :hook (on-init-ui-hook . (lambda ()
                             (eyebrowse-mode t)))
  :init
  (setq
   ;; since this used in meow config, must set here
   ;; if enable eyebrowse in on-init-ui-hook
   eyebrowse-keymap-prefix (kbd "C-c C-w")
   eyebrowse-new-workspace t
   eyebrowse-mode-line-style 'current)
  :config
  (defun +eyebrowse-switch-project ()
    "Switch to project in a new window config, project name will be used as config name.

No window config will created if the command is cancelled."
    (interactive)
    (let (succ)
      (unwind-protect
          (progn
            (eyebrowse-create-window-config)
            (call-interactively #'project-switch-project)
            (when-let ((proj (project-root (project-current))))
              (eyebrowse-rename-window-config
               (eyebrowse--get 'current-slot)
               (format "%s" (file-name-nondirectory (directory-file-name proj))))
              (setq succ t)))
        (unless succ
          (eyebrowse-close-window-config)))))
  ;; NOTE should update if `eyebrowse-keymap-prefix' changed
  (define-key eyebrowse-mode-map (kbd "C-c C-w l") #'+eyebrowse-switch-project)
  (define-key eyebrowse-mode-map (kbd "C-c C-w n") #'eyebrowse-create-named-window-config)


  (defun +eyebrowse-switch-named-window-config (name)
    "Switch to a window config with `NAME', or create it if not exist."
    (let* ((window-configs (eyebrowse--get 'window-configs))
           (current-slot (eyebrowse--get 'current-slot))
           (name-window-slot nil))
      (while window-configs
        (let* ((window-config (car window-configs))
               (window-name (nth 2 window-config)))
          (if (string= name window-name)
              (setq window-configs nil
                    name-window-slot (car window-config))
            (setq window-configs (cdr window-configs)))))
      (if name-window-slot
          (when (not (= current-slot name-window-slot))
            (eyebrowse-switch-to-window-config name-window-slot))
        (eyebrowse-create-window-config)
        (eyebrowse-rename-window-config
         (eyebrowse--get 'current-slot)
         name))))
  (advice-add 'xwidget-webkit-browse-url :before #'(lambda (url &optional new-session)
                                                     (+eyebrowse-switch-named-window-config "xwidget"))))

(eat-package window-numbering
  :straight (window-numbering :type git :host github :repo "DogLooksGood/window-numbering.el")
  :hook (on-init-ui-hook . (lambda ()
                             (require 'window-numbering)
                             (window-numbering-mode 1))))

(eat-package ace-window
  :straight t
  :commands
  ace-swap-window
  ace-window
  :init
  (setq aw-keys '(?a ?o ?e ?u ?i)
        aw-scope 'frame)
  (dolist (cmd '(ace-window
                 aw--select-window))
    (advice-add cmd :after #'my-pulse-momentary-line)))

(eat-package popper
  :straight t
  :hook (on-init-ui-hook . (lambda ()
                             (popper-mode +1)))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"

          "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup

          ;; help & message
          help-mode
          ghelp-page-mode
          compilation-mode))
  (with-eval-after-load 'project
    (setq popper-group-function 'popper-group-by-project))
  :config
  (global-set-key (kbd "M-`") #'popper-toggle-type)
  (defun my-popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 3)
     (floor (frame-height) 3)))
  (setq popper-window-height #'my-popper-fit-window-height))

;; When splitting window, show (other-buffer) in the new window

;; TODO drop dash.el
(defun +project-previous-buffer ()
  "Toggle to the previous buffer that belongs to current project."
  (if-let ((pr (project-current)))
      (->> (project--buffer-list pr)
           (--remove (or (minibufferp it)
                         (get-buffer-window-list it)))
           (car))))

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (+project-previous-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)


;; Rearrange split windows

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)


;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs

(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)


(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)

;; TODO init-editing-utils

(provide 'init-windows)
