;;; -*- lexical-binding: t -*-

;; Disable readline based native completion
(setq python-shell-completion-native-enable nil)

(defun pyrightconfig-write ()
  "Write a `pyrightconfig.json' file at the root of a project with
`venvPath` and `venv`."
  (interactive)
  (let* ((vp (string-trim-right python-shell-virtualenv-root "/"))
         (root (file-name-directory vp))
         (venv (file-name-base vp))
         (out-file (expand-file-name "pyrightconfig.json" root)))
    (with-temp-file out-file
      (insert (json-encode (list :venvPath root
                                 :venv venv))))
    (message "Configured `%s` to use environment `%s`" out-file pyvenv-virtual-env)))

(install-package 'pet)
;; This will turn on `pet-mode' on `python-mode' and `python-ts-mode'
(add-hook 'python-base-mode-hook 'pet-mode -10)

(provide 'init-python)
