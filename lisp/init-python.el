;;; -*- lexical-binding: t -*-

(leaf python
  :tag "builtin"
  :hook
  (python-mode-hook . flymake-mode)
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil))

(leaf highlight-indentation
  :straight t
  :hook
  (python-mode-hook . highlight-indentation-mode))

;; TODO use elpy + pyright
(leaf elpy
  :straight t
  :init
  (add-to-list 'elpy-modules 'elpy-module-autodoc)
  (add-to-list 'elpy-modules 'elpy-module-eldoc)
  :custom
  (elpy-rpc-virtualenv-path . 'current)
  (elpy-modules             . '(elpy-module-company
                                elpy-module-folding
                                elpy-module-yasnippet))
  :advice
  (:before python-mode elpy-enable))

(provide 'init-python)
