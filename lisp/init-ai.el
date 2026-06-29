;;; init-ai.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; gptel
;;
;; Store gpt key in ~/.authinfo:
;; machine api.openai.com login apikey password ****
(install-package 'gptel)

(setq gptel-default-mode 'org-mode
      gptel-org-branching-context t)
(with-eval-after-load 'gptel
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (add-hook 'gptel-mode-hook #'visual-fill-column-mode))

;;; gptel-commit
(install-package 'gptel-commit)

(with-eval-after-load 'magit
  (define-key git-commit-mode-map (kbd "C-c g") #'gptel-commit)
  (define-key git-commit-mode-map (kbd "C-c G") #'gptel-commit-rationale))

;;; emacs-codex-ide
(install-package 'emacs-codex-ide "https://github.com/dgillis/emacs-codex-ide")
(global-set-key (kbd "C-c C-;") #'codex-ide-menu)

;;; claude-code-ide
;; 号被封了🤡
(install-package 'claude-code-ide "https://github.com/manzaltu/claude-code-ide.el")

(setq claude-code-ide-terminal-backend 'ghostel)

(global-set-key (kbd "C-c C-'") #'claude-code-ide-menu)

(with-eval-after-load 'claude-code-ide
  ;; Enable Emacs MCP tools
  (claude-code-ide-emacs-tools-setup)

  (defun my/claude-code-ide-review-comment (comment)
    "把当前选区引用 + COMMENT 插入 Claude Code 输入框,每条占一行,不提交。
  适合一次写多条 comment,最后自己手动回车提交。
  不要求 Claude 窗口可见,也不会切换焦点。"
    (interactive (list (read-string "Comment: ")))
    (claude-code-ide-insert-at-mentioned)
    (sit-for 0.2)
    (let ((buffer (get-buffer (claude-code-ide--get-buffer-name))))
      (unless buffer
        (user-error "No Claude Code session for this project"))
      (with-current-buffer buffer
        (claude-code-ide--terminal-send-string (concat " " comment))
        (sit-for 0.1)))
    ;; 软换行:Claude Code 视为换行而非提交
    (claude-code-ide-insert-newline))

  ;; 加进 claude-code-ide 的 transient 菜单,放在 "i Insert selection" 后面
  (with-eval-after-load 'claude-code-ide-transient
    (transient-append-suffix 'claude-code-ide-menu "i"
      '("m" "Insert selection + comment" my/claude-code-ide-review-comment))))

;;; init-ai.el ends here
