;;; -*- lexical-binding: t -*-

;; Disable readline based native completion
(setq python-shell-completion-native-enable nil)

;; pyright with venv project, or check other pyright config like
;; extraPaths if project is managed by pdm and pyproject.toml, for
;; example:
;;
;; [tool.pyright]
;; extraPaths = ["__pypackages__/3.8/lib/"]
;;
;; https://github.com/microsoft/pyright/issues/2767
;; Also check basedpyright and delance
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

;;; pet, setup virtual environment for emacs
(install-package 'pet)
;; This will turn on `pet-mode' on `python-mode' and `python-ts-mode'
(add-hook 'python-base-mode-hook 'pet-mode -10)

;;; ruff, linter(use apheleia to format
(install-package 'flymake-ruff)

(defun my/flymake-ruff-maybe-enable ()
  (when (executable-find "ruff")
    (flymake-ruff-load)))
(add-hook 'python-base-mode-hook 'my/flymake-ruff-maybe-enable)

(provide 'init-python)
