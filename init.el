;;; init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Load pre init
;;
(defvar my/init-errors nil
  "Alist of (FILE . ERROR-STRING) for config files that errored while loading.
Collected so one broken module never aborts the rest of the configuration.")

(defun lakki.is/load-relative (file)
  "Load FILE relative to `user-emacs-directory'.
Any error is captured into `my/init-errors' and reported, but never
propagates -- so a single failure can no longer stop the remaining
modules (and their packages/config) from loading."
  (let ((rfile (expand-file-name file user-emacs-directory)))
    (when (file-exists-p rfile)
      (condition-case err
          (load rfile nil t t)
        (error
         (push (cons file (error-message-string err)) my/init-errors)
         (message "[init] ERROR loading %s: %s" file (error-message-string err)))))))
(lakki.is/load-relative "before-init.el")

;;; Load path
(setq custom-file (locate-user-emacs-file "custom.el"))
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)

;;; Built-in defaults
(lakki.is/load-relative "lisp/init-defaults.el")
(lakki.is/load-relative "lisp/init-editor.el")

;;; package.el
;;
;; Install into separate package dirs for each Emacs version, to prevent
;; bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))
(setq package-enable-at-startup nil)

;; Mirror bases tried in order. Each must serve gnu/ nongnu/ melpa/ beneath
;; it, so switching mirrors is just swapping the base. A refresh/download
;; failure transparently falls back to the next base -- this is what keeps a
;; single rate-limited or flaky mirror from breaking a from-scratch install.
(defvar my/package-mirror-bases
  '("https://mirrors.tuna.tsinghua.edu.cn/elpa/"  ; Tsinghua TUNA
    "https://mirrors.ustc.edu.cn/elpa/"           ; USTC
    "https://elpa.emacs-china.org/")              ; Emacs China
  "Ordered list of ELPA mirror base URLs to try.")

(defun my/use-package-mirror (base)
  "Point `package-archives' at the mirror rooted at BASE."
  (setq package-archives
        `(("gnu"    . ,(concat base "gnu/"))
          ("nongnu" . ,(concat base "nongnu/"))
          ("melpa"  . ,(concat base "melpa/")))))

(my/use-package-mirror (car my/package-mirror-bases))
(package-initialize)

(defvar my/package-refreshed nil
  "Non-nil once some mirror has successfully refreshed archive contents.")
(defvar my/failed-packages nil
  "Alist of (PKG . ERROR-STRING) for packages that could not be installed.")

(defun my/refresh-package-contents ()
  "Refresh archive contents, falling back through `my/package-mirror-bases'.
Return non-nil once a mirror succeeds."
  (catch 'done
    (dolist (base my/package-mirror-bases)
      (my/use-package-mirror base)
      (condition-case err
          (progn
            (package-refresh-contents)
            (setq my/package-refreshed t)
            (message "[init] package archives refreshed from %s" base)
            (throw 'done t))
        (error
         (message "[init] mirror failed (%s): %s -- trying next"
                  base (error-message-string err)))))
    (message "[init] WARNING: every package mirror failed to refresh")
    nil))

(defun install-package (pkg &optional url)
  "Ensure PKG is installed, resiliently.
With URL, install via `package-vc-install'.  Archives are refreshed
lazily on the first install; on failure we refresh again through the mirror
fallback and retry once; a package that still fails is recorded in
`my/failed-packages' instead of aborting init."
  (unless (package-installed-p pkg)
    (unless my/package-refreshed
      (my/refresh-package-contents))
    (condition-case _err
        (if url (package-vc-install url) (package-install pkg))
      (error
       (message "[init] install %s failed; refreshing + retrying once" pkg)
       (setq my/package-refreshed nil)
       (my/refresh-package-contents)
       (condition-case err2
           (if url (package-vc-install url) (package-install pkg))
         (error
          (push (cons pkg (error-message-string err2)) my/failed-packages)
          (message "[init] GAVE UP on %s: %s" pkg (error-message-string err2))))))))

;;; Benchmark
(install-package 'benchmark-init)
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)
(benchmark-init/activate)

;;; Configs
(lakki.is/load-relative "lisp/init-lib.el")

(lakki.is/load-relative "lisp/init-meow.el")
(lakki.is/load-relative "lisp/init-ui.el")
(lakki.is/load-relative "lisp/init-tools.el")
(lakki.is/load-relative "lisp/init-window.el")
(lakki.is/load-relative "lisp/init-chinese.el")
(lakki.is/load-relative "lisp/init-completion.el")

(lakki.is/load-relative "lisp/init-dev.el")
(lakki.is/load-relative "lisp/init-lisp.el")
(lakki.is/load-relative "lisp/init-go.el")
(lakki.is/load-relative "lisp/init-python.el")
(lakki.is/load-relative "lisp/init-web.el")

(lakki.is/load-relative "lisp/init-org.el")
(lakki.is/load-relative "lisp/init-git.el")
(lakki.is/load-relative "lisp/init-text.el")
(lakki.is/load-relative "lisp/init-mail.el")
(lakki.is/load-relative "lisp/init-shell.el")
(lakki.is/load-relative "lisp/init-tramp.el")
(lakki.is/load-relative "lisp/init-telega.el")
(lakki.is/load-relative "lisp/init-dired.el")
(lakki.is/load-relative "lisp/init-translator.el")
(lakki.is/load-relative "lisp/init-ai.el")

(when (eq system-type 'darwin)
  (lakki.is/load-relative "lisp/init-osx.el"))

;;; Load post init
(lakki.is/load-relative "after-init.el")

;;; init.el ends here
