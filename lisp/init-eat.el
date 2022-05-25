;;; -*- lexical-binding: t -*-

(defvar eat/fonts-default '("Roboto Mono" "Iosevka" "Menlo" "Source Code Pro")
  "First installed font will be set to default font.")
(defvar eat/fonts-unicode '("Apple Color Emoji" "Noto Color Emoji")
  "First installed font will be set to unicode font.")
(defvar eat/fonts-cn '("LXGW WenKai" "PingFang SC")
  "First installed font will be set to Chinese font.")
(defvar eat/fonts-variable-pitch '("Cardo" "Bookerly" "Nimbus Sans" "Helvetica")
  "First installed font will be set to variable font.")

(defvar eat/font-size 12
  "Default font size.")

(defvar eat/enable-icon t
  "Whether to enable `all-the-icons'.")

(defvar eat/theme 'modus-operandi
  "Default theme.")
(defvar eat/theme-tui 'carbon
  "Default theme in terminal.")
(defvar eat/theme-system-light 'modus-operandi
  "Default light theme after system appearance changed.")
(defvar eat/theme-system-dark 'spacemacs-dark
  "Default dark theme after system appearance changed.")
(defvar eat/theme-hooks nil
  "((theme-id . function) ...).")

(defvar eat/enable-benchmark nil
  "Enable `benchmark-init', run `benchmark-init/show-durations-tree' to see result.")

(defvar eat/user-full-name "Liu Bo")

(defvar eat/user-mail-address "liubolovelife@gmail.com")


(defun eat/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun eat/font-installed (list)
  "Return first installed font from LIST."
  (catch 'value
    (dolist (font list)
      (when (eat/font-installed-p font)
        (throw 'value font)))))

(defun eat/load-theme (theme)
  "Load THEME without confirm."
  (load-theme theme t))

(defun eat/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhance `load-theme' by disabling other enabled themes & calling hooks."
  (unless no-enable ;
    (mapc #'disable-theme custom-enabled-themes))
  (prog1
      (apply f theme-id t no-enable args)
    (unless no-enable ;
      (pcase (assq theme-id eat/theme-hooks)
        (`(,_ . ,f) (funcall f))))))
(advice-add 'load-theme :around #'eat/load-theme-advice)

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

(defun eat/show-startup-time ()
  "Print startup time."
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))
(add-hook 'emacs-startup-hook #'eat/show-startup-time)

;; GC automatically while unfocusing the frame
(add-function :after after-focus-change-function
              (lambda ()
                (unless (frame-focus-state)
                  (garbage-collect))))


(defconst eat/font-default (eat/font-installed eat/fonts-default))
(defconst eat/font-unicode (eat/font-installed eat/fonts-unicode))
(defconst eat/font-cn (eat/font-installed eat/fonts-cn))
(defconst eat/font-variable-pitch (eat/font-installed eat/fonts-variable-pitch))

(defconst eat/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst eat/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst eat/emacs29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")


;;; init-eat.el ends here
(provide 'init-eat)
