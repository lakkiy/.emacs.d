;;; -*- lexical-binding: t -*-

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;; replace `tab-bar'
(eat-package eyebrowse
  :straight t
  :hook (after-init-hook . (lambda ()
                             (eyebrowse-mode t)))
  :init
  ;; TODO select window config by name, create it if not exist
  ;;      open project in new workspace and rename to project name
  (setq
   eyebrowse-new-workspace t
   eyebrowse-mode-line-style 'current))

(eat-package window-numbering
  :straight (window-numbering :type git :host github :repo "DogLooksGood/window-numbering.el")
  :hook (after-init-hook . (lambda ()
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
  :hook (after-init-hook . (lambda ()
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
  (global-set-key (kbd "M-`") #'popper-cycle)
  (defun my-popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 3)
     (floor (frame-height) 3)))
  (setq popper-window-height #'my-popper-fit-window-height))

(defun eat-project-other-buffer-maybe ()
  "Return buffer in current project but not show in current windows, or run `other-buffer'."
  (if-let ((pr (project-current))
           (pb (car (cdr (project-buffers pr))))
           ((minibufferp pb)) ;; TODO auto return buffer in project but not minibuffer and in current window
           ((get-buffer-window-list pb)))
      pb
    (other-buffer)))

;; When splitting window, show (other-buffer) in the new window

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (eat-project-other-buffer-maybe))
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
