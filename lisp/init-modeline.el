;;; -*- lexical-binding: t -*-

(setq-default header-line-format nil)

(defun +smart-file-name-cached ()
  (if (eq (buffer-name) (car +smart-file-name-cache))
      (cdr +smart-file-name-cache)
    (let ((file-name (+smart-file-name)))
      (setq +smart-file-name-cache
            (cons (buffer-name) file-name))
      file-name)))

(defvar +smart-file-name-cache nil)

(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
This function is slow, so we have to use cache."
  (let ((vc-dir (vc-root-dir))
        (bfn (buffer-file-name (current-buffer))))
    (cond
     ((and bfn vc-dir)
      (file-relative-name bfn vc-dir))
     (bfn bfn)
     (t (buffer-name)))))

;; TODO use -*-FZSuXinShiLiuKaiS-R-GB-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1
;; to show flymake or flycheck errors count in mode line
(defun +format-mode-line ()
  (let* ((lhs '((:eval (when (fboundp 'eyebrowse-mode-line-indicator) (eyebrowse-mode-line-indicator)))
                (:eval (when (fboundp 'meow-indicator) (meow-indicator)))
                (:eval (when (fboundp 'rime-lighter) (rime-lighter)))
                " L%l C%C"
                (:eval (propertize " " 'display '(height 1.4))) ;; make mode line fill rime lighter height
                (:eval (propertize " " 'display '(raise -0.3)))
                (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line))
                (:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))))
         (rhs '((:eval (propertize (+smart-file-name-cached) 'face 'mode-line-buffer-id))
                " "
                (:eval mode-name)
                (vc-mode vc-mode)))
         (ww (window-width))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))

(setq-default mode-line-format '(:eval (+format-mode-line)))

(provide 'init-modeline)
