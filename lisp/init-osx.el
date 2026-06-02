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

;;; Find external programs
;;
;; Set PATH and `exec-path'
;; https://emacs-china.org/t/emacs-mac-port-profile/2895/29?u=rua
;; NOTE: When PATH is changed, run the following command
;; $ sh -c 'printf "%s" "$PATH"' > ~/.path
(defun my/getenv-path()
  (interactive)
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))

(when (file-exists-p "~/.path")
  (add-hook 'after-init-hook #'my/getenv-path))

;;; init-osx.el ends here
