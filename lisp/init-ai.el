;;; -*- lexical-binding: t -*-

;;; tabnine
(install-package 'tabnine)

(with-eval-after-load 'tabnine
  (add-hook 'kill-emacs-hook #'tabnine-kill-process)

  (defun +tabnine-disable-predicate()
    (or (meow-motion-mode-p)
        (meow-normal-mode-p)))
  (add-to-list 'tabnine-disable-predicates #'+tabnine-disable-predicate)

  (define-key tabnine-completion-map (kbd "TAB") nil)
  (define-key tabnine-completion-map (kbd "<tab>") nil)
  (define-key tabnine-completion-map (kbd "C-e") #'tabnine-accept-completion)
  (define-key tabnine-completion-map (kbd "C-g") #'tabnine-clear-overlay)
  (define-key tabnine-completion-map (kbd "M-p") #'tabnine-next-completion)
  (define-key tabnine-completion-map (kbd "M-n") #'tabnine-previous-completion))

;;; copilot
;;
;; Manually enable copilot, add the following code to custom.el:
;; (add-hook 'prog-mode-hook 'copilot-mode)

(install-package 'copilot "https://github.com/zerolfx/copilot.el")
(install-package 'copilot-chat "https://github.com/chep/copilot-chat.el")

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
(install-package 'gptel-quick "https://github.com/karthink/gptel-quick")

(setq gptel-default-mode 'org-mode
      gptel-org-branching-context t)
(add-hook 'gptel-mode-hook #'visual-fill-column-mode)

;;; init-ai.el ends here
