;;; -*- lexical-binding: t -*-

;; Edit and rename this file to custom.el

;; Enable copilot, also check `copilot-install-server' and `copilot-login'
;; (add-hook 'prog-mode-hook #'copilot-mode)

;; Set gptel backend
;; (with-eval-after-load 'gptel
;;   (setq gptel-backend (gptel-make-openai "SB OpenAI"
;;                         :host "api.openai-sb.com"
;;                         :key (retrieve-authinfo-key "api.openai-sb.com" "apikey")
;;                         :stream t
;;                         :models '("gpt-4"))))

;; Use gpt to generate commit message
;; (with-eval-after-load 'magit
;;   (require 'gpt-commit)
;;   (setq gpt-commit-api-url "https://api.openai-sb.com/v1/chat/completions")
;;   (setq gpt-commit-openai-key (retrieve-authinfo-key "api.openai-sb.com" "apikey"))
;;   (setq gpt-commit-model-name "gpt-4")
;;   (add-hook 'git-commit-setup-hook 'gpt-commit-message))
