;;; -*- lexical-binding: t -*-

(eat-package emacs-application-framework
  :straight (emacs-application-framework
             :type git
             :host github
             :repo "emacs-eaf/emacs-application-framework"
             :files ("*.el" "*.py" "core" "app"))
  :init
  (eat-package s :straight t)
  (eat-package ctable :straight t)
  (eat-package epc :straight t)
  (eat-package deferred :straight t)
  ;; TODO
  ;; (require 'eaf)

  (eat-package eaf-browser
    :straight (eaf-browser :type git :host github :repo "emacs-eaf/eaf-browser" :files ("*.el" "*.py"))
    :init
    (setq eaf-browser-continue-where-left-off t
          eaf-browser-enable-autofill t
          eaf-browser-enable-adblocker t)))

(eat-package chess :straight t)

(eat-package rainbow-mode
  :straight t
  :commands rainbow-mode)

(eat-package secret-mode
  :straight (secret-mode :type git :host github :repo  "bkaestner/secret-mode.el"))

(eat-package screenshot
  :straight (screenshot :type git :host github :repo "tecosaur/screenshot"))

(eat-package xterm-color
  :straight t
  :init
  ;; For shell and interpreters
  (setenv "TERM" "xterm-256color")
  ;; For compilation buffers
  (setq compilation-environment '("TERM=xterm-256color")))

(eat-package vterm
  :straight t
  :init
  (with-no-warnings
    (when (display-graphic-p)
      (defvar vterm-posframe--frame nil)

      (defun vterm-posframe-hidehandler (_)
        "Hidehandler used by `vterm-posframe-toggle'."
        (not (eq (selected-frame) posframe--frame)))

      (defun vterm-posframe-toggle ()
        "Toggle `vterm' child frame."
        (interactive)
        (let ((buffer (vterm--internal #'ignore 100)))
          (if (and vterm-posframe--frame
                   (frame-live-p vterm-posframe--frame)
                   (frame-visible-p vterm-posframe--frame))
              (progn
                (posframe-hide buffer)
                ;; Focus the parent frame
                (select-frame-set-input-focus (frame-parent vterm-posframe--frame)))
            (let ((width  (max 80 (/ (frame-width) 2)))
                  (height (/ (frame-height) 2)))
              (setq vterm-posframe--frame
                    (posframe-show
                     buffer
                     :poshandler #'posframe-poshandler-frame-center
                     :hidehandler #'vterm-posframe-hidehandler
                     :left-fringe 8
                     :right-fringe 8
                     :width width
                     :height height
                     :min-width width
                     :min-height height
                     :internal-border-width 3
                     :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                     ;; :background-color (face-background 'tooltip nil t)
                     :override-parameters '((cursor-type . t))
                     :accept-focus t))
              ;; Blink cursor
              (with-current-buffer buffer
                (save-excursion
                  (vterm-clear t))
                (setq-local cursor-type 'box))
              ;; Focus the child frame
              (select-frame-set-input-focus vterm-posframe--frame)))))
      (global-set-key (kbd "M-`") #'vterm-posframe-toggle))))

(eat-package popper
  :straight t
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
          compilation-mode))
  (popper-mode +1)
  (when (not (display-graphic-p))
    (global-set-key (kbd "M-`") #'popper-toggle-latest)))

(provide 'init-fun)
