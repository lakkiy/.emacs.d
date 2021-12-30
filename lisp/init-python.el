;;; -*- lexical-binding: t -*-

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

(eat-package jupyter :straight t)

(eat-package blacken
  :straight t
  :after python
  :init
  (setq blacken-skip-string-normalization t)
  :config
  ;; pip install black
  (add-hook 'python-mode-hook 'blacken-mode))

(eat-package pyimport
  :straight t
  :after python
  :config
  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "C-c C-i") #'pyimport-insert-missing)
    (define-key python-mode-map (kbd "C-c C-r") #'pyimport-remove-unused)))

(eat-package python-isort
  :straight (python-isort :type git :host github :repo "wyuenho/emacs-python-isort")
  :after python
  :init
  :config
  ;; pip install isort
  (add-hook 'python-mode-hook 'python-isort-on-save-mode))

(eat-package live-py-mode
  :straight t
  :after python
  :config
  ;; pip install twisted ?
  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "C-c l") 'live-py-mode)))

(eat-package conda
  :straight t
  :config
  ;; conda TODO macos miniconda var setup
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(provide 'init-python)
