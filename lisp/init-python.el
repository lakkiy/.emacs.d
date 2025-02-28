;;; init-python.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; venv
(install-package 'pet)

;; Have performance issue with over tramp, and seems don't work with conda.
(defun my/maybe-enable-pet-mode ()
  (unless (file-remote-p default-directory)
    (pet-mode 1)))
(add-hook 'python-base-mode-hook 'my/maybe-enable-pet-mode -10)

;;; pytest
(install-package 'python-pytest)

;;; ruff
;;
;; lint and format python code(use apheleia to do format in emacs
(install-package 'flymake-ruff)

(defun my/flymake-ruff-maybe-enable ()
  (when (executable-find "ruff")
    (flymake-ruff-load)))
(add-hook 'python-base-mode-hook 'my/flymake-ruff-maybe-enable)

;;; jupyter
;;
;; Better with jupytext and pandoc installed.
(install-package 'code-cells)
(add-hook 'python-base-mode-hook 'code-cells-mode-maybe)

;;; init-python.el ends here
