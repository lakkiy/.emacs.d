;;; -*- lexical-binding: t -*-

(setq ispell-program-name "aspell"
      ispell-extra-args '( "-W" "3" "--sug-mode=ultra" "--lang=en_US"
                           ;; run-together allows compound words
                           ;; like "viewport".
                           "--run-together"))

;; jit-spell
;;
;; jit-spell relies on the =ispell= library to pick a spell checking
;; program and dictionaries.
;; TODO check aspell and directory exists
(install-package 'jit-spell)

;;; init-spell.el ends here
(provide 'init-spell)
