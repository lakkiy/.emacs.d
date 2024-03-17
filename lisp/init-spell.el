;;; -*- lexical-binding: t -*-

(setq ispell-program-name "aspell"
      ispell-extra-args '( "-W" "3" "--sug-mode=ultra" "--lang=en_US"
                           ;; run-together allows compound words
                           ;; like "viewport".
                           "--run-together"))

;;; jit-spell
;;
;; jit-spell relies on the =ispell= library to pick a spell checking
;; program and dictionaries.
(install-package 'jit-spell)
(when (executable-find "aspell")
  (add-hook 'text-mode-hook 'jit-spell-mode)
  (add-hook 'prog-mode-hook 'jit-spell-mode))

(provide 'init-spell)
