;;; -*- lexical-binding: t -*-

;; Disable readline based native completion
(setq python-shell-completion-native-enable nil
      python-indent-guess-indent-offset nil)

;; pyright with pdm + project.toml/pyrightconfig.json projet.
;; [tool.pyright]
;; extraPaths = ["__pypackages__/3.8/lib/", "src/]
;; https://pdm-project.org/en/latest/usage/pep582/#__tabbed_1_2
;; https://github.com/microsoft/pyright/issues/2767
;;
;; pyright with venv project, need setup venv and venvPath
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

;;; venv support
;;
;; don't need this if use pdm to manage python project
(install-package 'pyvenv)

;;; ruff, lint and format python code(use apheleia to do format in emacs
(install-package 'flymake-ruff)

(defun my/flymake-ruff-maybe-enable ()
  (when (executable-find "ruff")
    (flymake-ruff-load)))
(add-hook 'python-base-mode-hook 'my/flymake-ruff-maybe-enable)

(provide 'init-python)
