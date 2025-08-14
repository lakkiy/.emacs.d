;;; corfu
(install-package corfu)
(install-package popon)
(install-package corfu-terminal)

;; (add-hook 'after-init-hook #'(lambda () (global-corfu-mode 1)))
(add-hook 'corfu-mode-hook #'(lambda ()
                               (unless (display-graphic-p)
                                 (corfu-terminal-mode +1))
                               (corfu-popupinfo-mode)))

(setq corfu-preview-current nil
      corfu-auto-delay 0.2
      corfu-auto-prefix 2
      corfu-quit-no-match t
      corfu-quit-at-boundary t
      corfu-auto t)

(with-eval-after-load 'corfu
  ;; company can quit capf and insert mode without config
  (keymap-set corfu-map "<escape>" #'(lambda ()
                                       (interactive)
                                       (corfu-quit)
                                       (when (meow-insert-mode-p)
                                         (meow-insert-exit))))
  (keymap-set corfu-map "RET" nil))

(defun eat/yas-next-field-or-maybe-expand ()
  "Try complete current cond or `yas-next-field-or-maybe-expand'.

Sometime lsp client return a snippet and complete didn't work(TAB will jump to next field),
so try complete filst, if there nothing to complete then try to jump to next field or expand."
  (interactive)
  (or (corfu-insert) ;; NOTE this works
      (yas-next-field-or-maybe-expand)))
(with-eval-after-load 'yasnippet
  (keymap-set yas-keymap "<tab>" 'eat/yas-next-field-or-maybe-expand)
  (keymap-set yas-keymap "TAB" 'eat/yas-next-field-or-maybe-expand))

;;; window-numbering
(install-package 'window-numbering)
(add-hook 'after-init-hook #'window-numbering-mode)

;; NOTE this break query-replace on emacs30
;; The problem of the default query-replace UI is when you accidently
;; press a key that's not in query-replace-map, the session is
;; terminated. This makes it feel fragile.
;;
;; Here's an advice fixing it. When you press a non query-replace-map
;; key, it opens the help info.
;;
;; Stole from https://github.com/astoff/isearch-mb/wiki
(define-advice perform-replace (:around (fn &rest args) dont-exit-on-anykey)
  "Don't exit replace for anykey that's not in `query-replace-map'."
  (cl-letf* ((lookup-key-orig
              (symbol-function 'lookup-key))
             ((symbol-function 'lookup-key)
              (lambda (map key &optional accept-default)
                (or (apply lookup-key-orig map key accept-default)
                    (when (eq map query-replace-map) 'help)))))
    (apply fn args)))

;; sidebar
;;
;; file tree
(install-package 'dired-sidebar)
(setq dired-sidebar-theme 'ascii)

;; bing ai search
;;
;; - Install the cookie editor extension for [[https://microsoftedge.microsoft.com/addons/detail/cookieeditor/neaplmfkghagebokkhpjpoebhdledlfi][Egde]]
;; - Go to bing.com
;; - Open the extension
;; - Click “Export” on the bottom right (This saves your cookies to clipboard)
;; - Paste your cookies into a file cookies.json
;; - Set =aichat-bingai-cookies-file= to your cookies.json path
(install-package 'async-await)
(install-package 'emacs-aichat "https://github.com/xhcoding/emacs-aichat")

(setq aichat-bingai-cookies-file "~/Dropbox/.bingcookies.json"
      aichat-bingai-chat-file "~/Sync/aichat.md")
(autoload #'aichat-bingai-chat "aichat-bingai.el" nil t)
(autoload #'aichat-bingai-assistant "aichat-bingai.el" nil t)


;; windmove
;;
;; If the keybinding is conflict with window mamager, try frames-only-mode.
(keymap-global-set "s-p" 'windmove-up)
(keymap-global-set "s-h" 'windmove-left)
(keymap-global-set "s-t" 'windmove-right)
(keymap-global-set "s-n" 'windmove-down)


;; my/ctl-t-map
;;
;; The original `C-t' is bound to `transpose-chars', which is not very
;; useful(I never use it since I use emacs), and `C-t' is only for my
;; personal keymap in dvorak keyboard layout.
(define-prefix-command 'my/ctl-t-map)
(global-set-key (kbd "C-t") 'my/ctl-t-map)


;; Highlight current line
;;
;; Restrict `hl-line-mode' highlighting to the current window, reducing visual
;; clutter and slightly improving `hl-line-mode' performance.
(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)

(defun my/hl-line-setup ()
  "Disable `hl-line-mode' if region is active."
  (when (and (bound-and-true-p hl-line-mode)
             (region-active-p))
    (hl-line-unhighlight)))
(with-eval-after-load 'hl-line
  (add-hook 'post-command-hook #'my/hl-line-setup))
(when (display-graphic-p)
  (add-hook 'prog-mode-hook #'hl-line-mode))

;; NOTE 会卡住编辑，不太好用
;; Hide org-mode property.
(install-package 'org-tidy)
(setq org-tidy-properties-style 'invisible)
(add-hook 'org-mode-hook #'org-tidy-mode)

;; database
(install-package 'pg)
(install-package 'pgmacs "https://github.com/emarsden/pgmacs")

;; pdf-tools
(install-package 'pdf-tools)

(autoload #'pdf-view-mode "pdf-tools")
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(setq-default pdf-view-display-size 'fit-page)
;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)

(with-eval-after-load "pdf-tools"
  (pdf-tools-install-noverify)
  (keymap-substitute pdf-view-mode-map #'scroll-up-command #'pdf-view-scroll-up-or-next-page)
  (keymap-substitute pdf-view-mode-map #'scroll-down-command #'pdf-view-scroll-down-or-previous-page))

;; jit lock error in go ts mode after save file
(install-package 'outli "https://github.com/jdtsmith/outli")
(add-hook 'prog-mode-hook #'(lambda () (unless (file-remote-p default-directory) (outli-mode 1))))

;; 加上之后 project find file 不忽略 gitigrone 的文件了
(defun my/project-try-local (dir)
  "Determine if DIR is a non-Git project."
  (catch 'ret
    (let ((pr-flags '((".project")
                      ("go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
                      ("Makefile" "README.org" "README.md"))))
      (dolist (current-level pr-flags)
        (dolist (f current-level)
          (when-let ((root (locate-dominating-file dir f)))
            (throw 'ret (cons 'local root))))))))
(setq project-find-functions '(my/project-try-local project-try-vc))

;; NOTE use rime-regexp
(install-package 'pinyinlib)

;; https://emacs-china.org/t/vertico/17913
(defun completion--regex-pinyin (str)
  (orderless-regexp (pinyinlib-build-regexp-string str)))
(with-eval-after-load 'orderless
  (require 'pinyinlib)
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; NOTE 如果只在 minibuffer 中使用 orderless 会导致 rime-regexp 无法正常工作
(defun sanityinc/use-orderless-in-minibuffer ()
  (setq-local completion-styles '(substring orderless)))
(add-hook 'minibuffer-setup-hook #'sanityinc/use-orderless-in-minibuffer)

;; NOTE 目前测试没问题， tramp 后颜色正常显示
;; 不知道为什么现在在 tramp 上执行 vc-region-history
;; 乱码或者颜色不对，例如 ^[33m，目前先用这个函数救急
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

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

;;; delta
;; 搞的 magit 太卡了
(install-package 'magit-delta)

(when (executable-find "delta")
  (add-hook 'magit-mode-hook #'magit-delta-mode))

(install-package 'aider)
(setq aider-args '("--no-auto-commits" "--cache-prompts" "--model" "deepseek/deepseek-chat"))
(with-eval-after-load 'aider
  (setenv "DEEPSEEK_API_KEY" (retrieve-authinfo-key "api.deepseek.com" "apikey")))


;;; flyover
;; 竟然强制安装 flycheck
(install-package 'flyover)

(setq flyover-checkers '(flymake))

(add-hook 'flymake-mode-hook #'flyover-mode)

;; 从没用过，不好配置，而且还可能用 gptel-aibo 代替
;;; minuet
;;
;; For default `minuet-openai-fim-compatible-options', make sure env
;; DEEPSEEK_API_KEY was set.
;; Manually enable copilot, add the following code to post-init.el:
;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
;;
(install-package 'minuet)

(with-eval-after-load 'minuet
  (keymap-set minuet-active-mode-map "C-g" #'minuet-dismiss-suggestion)
  (keymap-set minuet-active-mode-map "C-e" #'minuet-accept-suggestion)
  (keymap-set minuet-active-mode-map "M-p" #'minuet-previous-suggestion)
  (keymap-set minuet-active-mode-map "M-n" #'minuet-next-suggestion))

;; claude-code-ide 看起来更好用
;;; claude-code
;;
(install-package 'claude-code "https://github.com/stevemolitor/claude-code.el")

;; make the Claude window appear in a persistent side window on the right side
;; of with 33% width
(add-to-list 'display-buffer-alist
             '("^\\*claude"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 90)))

(add-hook 'after-init-hook 'claude-code-mode)

;; rsync
;; 用 dirvish-rsync 替代原生的 dired-rsync
(install-package 'dired-rsync)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-r") 'dired-rsync))
