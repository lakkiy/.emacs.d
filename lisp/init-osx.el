;;; init-osx.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Have to enable menu bar on mac port, otherwise emacs lost focus.
(when (display-graphic-p)
  (menu-bar-mode 1))

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;; https://github.com/d12frosted/homebrew-emacs-plus/issues/383
(setq insert-directory-program "gls")

;; `save-buffers-kill-emacs' will shutdown emacs daemon.
(global-set-key [(super q)] #'save-buffers-kill-terminal)
(global-set-key [(super a)] #'mark-whole-buffer)
(global-set-key [(super v)] #'yank)
(global-set-key [(super c)] #'kill-ring-save)
(global-set-key [(super s)] #'save-buffer)
(global-set-key [(super w)] #'delete-frame)
(global-set-key [(super z)] #'undo)

(setq ns-use-native-fullscreen nil
      ;; Render thinner fonts
      ns-use-thin-smoothing t
      ;; Don't open a file in a new frame
      ns-pop-up-frames nil)

;; https://github.com/d12frosted/homebrew-emacs-plus/issues/323#issuecomment-2762214714
;; 虽然这样可以解决找不到 libgccjit 的问题，但是由于我安装了 nix，带了一个 linux
;; version 的 ld，导致 native compile 调用 libgccjit 的使用报错 unknown argument，
;; 暂时不解决这个问题，直接关掉 native compile
(defun homebrew-gcc-paths ()
  "Return GCC library paths from Homebrew installations.
Detects paths for gcc and libgccjit packages to be used in LIBRARY_PATH."
  (let* ((paths '())
         (brew-bin (or (executable-find "brew")
                       (let ((arm-path "/opt/homebrew/bin/brew")
                             (intel-path "/usr/local/bin/brew"))
                         (cond
                          ((file-exists-p arm-path) arm-path)
                          ((file-exists-p intel-path) intel-path))))))

    (when brew-bin
      ;; Get gcc paths.
      (let* ((gcc-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix gcc"))))
             (gcc-lib-current (expand-file-name "lib/gcc/current" gcc-prefix)))
        (push gcc-lib-current paths)

        ;; Find apple-darwin directory.
        (let* ((default-directory gcc-lib-current)
               (arch-dirs (file-expand-wildcards "gcc/*-apple-darwin*/*[0-9]")))
          (when arch-dirs
            (push (expand-file-name
                   (car (sort arch-dirs #'string>)))
                  paths))))

      ;; Get libgccjit paths
      (let* ((jit-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix libgccjit"))))
             (jit-lib-current (expand-file-name "lib/gcc/current" jit-prefix)))
        (push jit-lib-current paths)))

    (nreverse paths)))

(defun setup-macos-native-comp-library-paths ()
  "Set up LIBRARY_PATH for native compilation on macOS.
Includes Homebrew GCC paths and CommandLineTools SDK libraries."
  (let* ((existing-paths (split-string (or (getenv "LIBRARY_PATH") "") ":" t))
         (gcc-paths (homebrew-gcc-paths))
         (clt-paths '("/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))
         (unique-paths (delete-dups
                        (append existing-paths gcc-paths clt-paths))))

    (setenv "LIBRARY_PATH" (mapconcat #'identity unique-paths ":"))))

;; Set up library paths for native compilation on macOS.
(when (eq system-type 'darwin)
  (setup-macos-native-comp-library-paths))

;;; init-osx.el ends here
