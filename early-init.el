;;; early-init.el --- before Emacs create frame -*- no-byte-compile: t; lexical-binding: t; -*-

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(let ((old-file-name-handler-alist file-name-handler-alist))
  ;; `file-name-handler-alist' is consulted on each `require', `load' and
  ;; various path/io functions. You get a minor speed up by unsetting this.
  ;; Some warning, however: this could cause problems on builds of Emacs where
  ;; its site lisp files aren't byte-compiled and we're forced to load the
  ;; *.el.gz files (e.g. on Alpine).
  (setq-default file-name-handler-alist nil)
  ;; ...but restore `file-name-handler-alist' later, because it is needed for
  ;; handling encrypted or compressed files, among other things.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)
              (setq file-name-handler-alist
                    ;; Merge instead of overwrite because there may have bene changes to
                    ;; `file-name-handler-alist' since startup we want to preserve.
                    (delete-dups (append file-name-handler-alist
                                         old-file-name-handler-alist))))
            t))

;;; early-init.el ends here
