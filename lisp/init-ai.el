;;; init-ai.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; copilot
;;
;; Manually enable copilot, add the following code to post-init.el:
;; (add-hook 'prog-mode-hook 'copilot-mode)
;;
(install-package 'copilot)
(install-package 'copilot-chat)

;; 由于 `lisp-indent-offset' 的默认值是 nil，在编辑 elisp 时每敲一个字
;; 符都会跳出一个 warning，将其默认值设置为 t 以永不显示这个 warning
(setq-default copilot--indent-warning-printed-p t
              copilot-indent-offset-warning-disable t)

(with-eval-after-load 'copilot
  ;; 文件超出 `copilot-max-char' 的时候不要弹出一个 warning 的 window
  (defun my-copilot-get-source-suppress-warning (original-function &rest args)
    "Advice to suppress display-warning in copilot--get-source."
    (cl-letf (((symbol-function 'display-warning) (lambda (&rest args) nil)))
      (apply original-function args)))
  (advice-add 'copilot--get-source :around #'my-copilot-get-source-suppress-warning)

  (add-to-list 'copilot-major-mode-alist '("go" . "go"))
  (add-to-list 'copilot-major-mode-alist '("go-ts" . "go"))

  (keymap-set copilot-completion-map "C-g" #'copilot-clear-overlay)
  (keymap-set copilot-completion-map "C-e" #'copilot-accept-completion)
  (keymap-set copilot-completion-map "M-p" #'copilot-previous-completion)
  (keymap-set copilot-completion-map "M-n" #'copilot-next-completion)

  ;; only enable copilot in meow insert mode
  (with-eval-after-load 'meow
    (add-to-list 'copilot-enable-predicates 'meow-insert-mode-p)))

;;; gptel
;;
;; Store gpt key in ~/.authinfo:
;; machine api.openai.com login apikey password ****
;;
;; Or other host proxy to openai:
;; machine api.openai-sb.com login apikey password ****
;; And custom gptel backend:
;; (with-eval-after-load 'gptel
;;   (setq gptel-backend (gptel-make-openai "SB OpenAI"
;;                         :host "api.openai-sb.com"
;;                         :key (retrieve-authinfo-key "api.openai-sb.com" "apikey")
;;                         :stream t
;;                         :models '("gpt-4"))))
(install-package 'gptel)

(setq gptel-default-mode 'org-mode
      gptel-org-branching-context t)
(with-eval-after-load 'gptel
  (add-hook 'gptel-mode-hook #'visual-fill-column-mode))


(install-package 'aidermacs)
(setq aidermacs-default-model "deepseek/deepseek-chat")
(setenv "DEEPSEEK_API_KEY" (retrieve-authinfo-key "api.deepseek.com" "apikey"))


;;; init-ai.el ends here
