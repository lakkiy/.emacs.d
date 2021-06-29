;;; -*- lexical-binding: t -*-

(straight-use-package 'elpy)
(straight-use-package 'blacken)
(straight-use-package 'live-py-mode)
(straight-use-package '(python-isort :type git :host github :repo "wyuenho/emacs-python-isort"))
(straight-use-package 'pyimport)
(straight-use-package 'jupyter)
(straight-use-package 'conda)

(+pdump-packages 'elpy
                 'conda
                 'jupyter
                 'blacken
                 'live-py-mode
                 'python-isort
                 'pyimport)

(defvar python--tools '("isort"
                        "black"))

(defun python-update-tools ()
  (interactive)
  (unless (executable-find "pip3")
    (user-error "Unable to find `pip3' in `exec-path'!"))
  (message "Installing python tools...")
  (let ((proc-name "python-tools")
        (proc-buffer "*Python Tools*"))
    (dolist (pkg python--tools)
      (set-process-sentinel
       (start-process proc-name proc-buffer "pip3" "install" pkg)
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed %s" pkg)
             (message "Failed to install %s: %d" pkg status))))))))

;;; elpy
;; pip install pylint
;; pip install flake8 ?
(setq
 elpy-rpc-python-command "python3")

(advice-add 'python-mode :before 'elpy-enable)

;;; blacken
(setq
 blacken-skip-string-normalization t)

(with-eval-after-load "python"
  ;;; pyimport
  (define-key python-mode-map (kbd "C-c C-i") #'pyimport-insert-missing)
  (define-key python-mode-map (kbd "C-c C-r") #'pyimport-remove-unused)
  ;;; python-isort
  ;; pip install isort
  (add-hook 'python-mode-hook 'python-isort-on-save-mode)
  ;;; live-py-mode
  ;; pip install twisted ?
  (define-key python-mode-map (kbd "C-c l") 'live-py-mode)
  ;;; blacken - reformat python buffer
  ;; format evere time you save
  ;; pip install black
  (add-hook 'python-mode-hook 'blacken-mode))

(setq
 python-indent-offset 4
 python-shell-completion-native-enable nil
 python-shell-interpreter "python3"
 python-indent-guess-indent-offset nil)

;;; conda TODO macos miniconda var setup
(with-eval-after-load "conda"
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(provide 'init-python)
