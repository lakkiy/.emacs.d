;;; -*- lexical-binding: t -*-
;;
;; 一些工具函数

(defun get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun format-second-timestamp (begin end)
  "Convert the selected region (a timestamp in seconds) to a formatted time string."
  (interactive "r")
  (let* ((timestamp-str (buffer-substring-no-properties begin end))
         (timestamp (string-to-number timestamp-str))
         (formatted-time (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time timestamp))))
    (message "%s" formatted-time)))

(defun eat/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha-background) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha-background newalpha))))))
(global-set-key (kbd "M-C-8") (lambda () (interactive) (eat/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (eat/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha-background . 100)))))

;; 不知道为什么现在在 tramp 上执行 vc-region-history 或者 urgrep 都会有
;; 乱码或者颜色不对，例如 ^[33m，目前先用这个函数救急
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;;; Delete things(don’t send to kill ring
(defun eat/delete-to-the-begining ()
  (interactive)
  (delete-region (point-min) (point)))

(defun eat/delete-to-the-end ()
  (interactive)
  (delete-region (point) (point-max)))

(defun eat/delete-whole-buffer ()
  (interactive)
  (delete-region (point-min) (point-max)))

(defun eat/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Load $PATH
;; https://emacs-china.org/t/emacs-mac-port-profile/2895/29?u=rua
;; NOTE: When PATH is changed, run the following command
;; $ sh -c 'printf "%s" "$PATH"' > ~/.path
;; then add this to custom.el
;; (add-hook 'after-init-hook #'eat/getenv-path)
(defun my/getenv-path()
  (interactive)
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))

(if (file-exists-p "~/.path")
    (add-hook 'after-init-hook #'my/getenv-path)
  (message "%s" "Run '$ sh -c 'printf \"%s\" \"$PATH\"' > ~/.path' in your terminal then M-x eat/getenv-path."))

;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(keymap-substitute global-map #'move-beginning-of-line #'smarter-move-beginning-of-line)

;;; window
;;
;; When splitting window, show (other-buffer) in the new window

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(keymap-global-set "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically))
(keymap-global-set "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(keymap-global-set "C-x 1" 'sanityinc/toggle-delete-other-windows)

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

(keymap-global-set "C-x |" 'split-window-horizontally-instead)
(keymap-global-set "C-x _" 'split-window-vertically-instead)

;; The problem of the default query-replace UI is when you accidently
;; press a key that's not in query-replace-map, the session is
;; terminated. This makes it feel fragile.
;;
;; Here's an advice fixing it. When you press a non query-replace-map
;; key, it opens the help info.
;;
;; Stole from https://github.com/astoff/isearch-mb/wiki
(define-advice perform-replace (:around (fn &rest args) dont-exit-on-anykey)
  "Don't exit replace for anykey that's not in `query-replace-map'."
  (cl-letf* ((lookup-key-orig
              (symbol-function 'lookup-key))
             ((symbol-function 'lookup-key)
              (lambda (map key &optional accept-default)
                (or (apply lookup-key-orig map key accept-default)
                    (when (eq map query-replace-map) 'help)))))
    (apply fn args)))

(provide 'init-utils)
