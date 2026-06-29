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

;;;; Hl-line
;;
;; 太遮挡视线了
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
(add-hook 'prog-mode-hook #'hl-line-mode)

;; 会让我的画面跳来跳去
;; https://www.reddit.com/r/emacs/comments/1ltp2j0/comment/n1u4rv1/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(setq flymake-show-diagnostics-at-end-of-line 'fancy)

;;; eglot-booster
;; 现有的电脑已经跑的很快了，暂时不需要
(install-package 'eglot-booster "https://github.com/jdtsmith/eglot-booster")
(when (executable-find "emacs-lsp-booster")
  (setq eglot-booster-no-remote-boost t
        eglot-booster-io-only t)
  (add-hook 'after-init-hook #'eglot-booster-mode))

;;; lsp-bridge
;; 用不上
(install-package 'lsp-bridge "https://github.com/manateelazycat/lsp-bridge")
(install-package 'flymake-bridge "https://github.com/liuyinz/flymake-bridge")

;; NOTE otherwise this may cause lsp-bridge-ref buffer didn't show
(setq window-resize-pixelwise nil)

(setq acm-enable-tabnine nil ;; default is t
      lsp-bridge-c-lsp-server "ccls")

(defun my/lsp-bridge-mode-setup ()
  (interactive)
  (flymake-bridge-setup)
  ;; Disable corfu since lsp-bridge use acm.
  (ignore-errors
    (company-mode -1)
    (corfu-mode -1))
  ;; Use tab to jump to next field but do complete when there's acm complete.
  (with-eval-after-load 'yasnippet
    (define-key yas-keymap (kbd "<tab>") 'acm-complete-or-expand-yas-snippet)
    (define-key yas-keymap (kbd "TAB") 'acm-complete-or-expand-yas-snippet)))

(with-eval-after-load 'lsp-bridge
  (add-hook 'lsp-bridge-mode-hook #'my/lsp-bridge-mode-setup)

  (keymap-set lsp-bridge-mode-map "M-."     #'lsp-bridge-find-def)
  (keymap-set lsp-bridge-mode-map "C-x 4 ." #'lsp-bridge-find-def-other-window)
  (keymap-set lsp-bridge-mode-map "M-,"     #'lsp-bridge-find-def-return)
  (keymap-set lsp-bridge-mode-map "M-?"     #'lsp-bridge-find-references)
  (keymap-set lsp-bridge-mode-map "M-'"     #'lsp-bridge-find-impl)
  (keymap-set lsp-bridge-mode-map "C-c r"   #'lsp-bridge-rename)
  (keymap-set lsp-bridge-mode-map "M-RET"   #'lsp-bridge-code-action)
  (keymap-set lsp-bridge-ref-mode-map "p"   #'lsp-bridge-ref-jump-prev-file)
  (keymap-set lsp-bridge-ref-mode-map "h"   #'lsp-bridge-ref-jump-prev-keyword)
  (keymap-set lsp-bridge-ref-mode-map "t"   #'lsp-bridge-ref-jump-next-keyword)
  (keymap-set lsp-bridge-ref-mode-map "n"   #'lsp-bridge-ref-jump-next-file)
  (keymap-set lsp-bridge-ref-mode-map "j" nil)
  (keymap-set lsp-bridge-ref-mode-map "k" nil)
  (keymap-set lsp-bridge-ref-mode-map "h" nil)
  (keymap-set lsp-bridge-ref-mode-map "l" nil))

;;; breadcrumb
;;
;; Display function name on header
(install-package 'breadcrumb)

;; 不知道是啥了
(install-package 'sly)
(setq inferior-lisp-program "sbcl")

;; 不学 clojure 了
(install-package 'clojure-mode)
(install-package 'cider)
(install-package 'clj-refactor)

(add-hook 'clojure-mode-hook #'puni-mode)

(with-eval-after-load 'clojure-mode
  ;; better indentation for compojure
  ;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
  (define-clojure-indent
   (defroutes 'defun)
   (GET 2)
   (POST 2)
   (PUT 2)
   (DELETE 2)
   (HEAD 2)
   (ANY 2)
   (OPTIONS 2)
   (PATCH 2)
   (rfn 2)
   (let-routes 1)
   (context 2)))

;; 不在 emacs 中安装
;; Install or update tools
(defvar go--tools '("golang.org/x/tools/gopls"
                    "golang.org/x/tools/cmd/goimports"
                    "honnef.co/go/tools/cmd/staticcheck"
                    "github.com/go-delve/delve/cmd/dlv"
                    "github.com/zmb3/gogetdoc"
                    "github.com/josharian/impl"
                    "github.com/cweill/gotests/..."
                    "github.com/fatih/gomodifytags"
                    "github.com/davidrjenni/reftools/cmd/fillstruct"
                    "github.com/rogpeppe/godef")
  "All necessary go tools.")

(defun go-update-tools ()
  "Install or update go tools."
  (interactive)
  (unless (executable-find "go")
    (user-error "Unable to find `go' in `exec-path'!"))

  (message "Installing go tools...")
  (dolist (pkg go--tools)
    (set-process-sentinel
     (start-process "go-tools" "*Go Tools*" "go" "install" "-v" "-x" (concat pkg "@latest"))
     (lambda (proc _)
       (let ((status (process-exit-status proc)))
         (if (= 0 status)
             (message "Installed %s" pkg)
           (message "Failed to install %s: %d" pkg status)))))))

;; 用不上
(defun my/format-go ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (shell-command "git diff --name-only --cached | grep '\.go$' | xargs -I {} goimports -w {}")))
(keymap-set project-prefix-map "t" #'my/format-go)

;;; jupyter
;;
;; Better with jupytext and pandoc installed.
;; 暂时用不上
(install-package 'code-cells)
(add-hook 'python-base-mode-hook 'code-cells-mode-maybe)

;;; copilot
;;
;; Manually enable copilot, add the following code to post-init.el:
;; (add-hook 'prog-mode-hook 'copilot-mode)
;;
;; AI 时代不需要了
(install-package 'copilot)

;; 由于 `lisp-indent-offset' 的默认值是 nil，在编辑 elisp 时每敲一个字
;; 符都会跳出一个 warning，将其默认值设置为 t 以永不显示这个 warning
(setq-default copilot--indent-warning-printed-p t
              copilot-indent-offset-warning-disable t)

(with-eval-after-load 'copilot
  ;; 文件超出 `copilot-max-char' 的时候不要弹出一个 warning 的 window
  (defun lakki.is/copilot-get-source-suppress-warning (original-function &rest args)
    "Advice to suppress display-warning in copilot--get-source."
    (cl-letf (((symbol-function 'display-warning) (lambda (&rest args) nil)))
      (apply original-function args)))
  (advice-add 'copilot--get-source :around #'lakki.is/copilot-get-source-suppress-warning)

  (add-to-list 'copilot-major-mode-alist '("go" . "go"))
  (add-to-list 'copilot-major-mode-alist '("go-ts" . "go"))

  (keymap-set copilot-completion-map "C-g" #'copilot-clear-overlay)
  (keymap-set copilot-completion-map "C-e" #'copilot-accept-completion)
  (keymap-set copilot-completion-map "M-p" #'copilot-previous-completion)
  (keymap-set copilot-completion-map "M-n" #'copilot-next-completion)

  ;; only enable copilot in meow insert mode
  (with-eval-after-load 'meow
    (add-to-list 'copilot-enable-predicates 'meow-insert-mode-p)))


;; winum
;;
;; 不好用，window 的 number 经常变
(install-package 'winum)

(setq winum-scope 'visible)

(keymap-global-set "M-0" #'winum-select-window-0-or-10)
(keymap-global-set "M-1" #'winum-select-window-1)
(keymap-global-set "M-2" #'winum-select-window-2)
(keymap-global-set "M-3" #'winum-select-window-3)
(keymap-global-set "M-4" #'winum-select-window-4)
(keymap-global-set "M-5" #'winum-select-window-5)
(keymap-global-set "M-6" #'winum-select-window-6)
(keymap-global-set "M-7" #'winum-select-window-7)
(keymap-global-set "M-8" #'winum-select-window-8)
(keymap-global-set "M-9" #'winum-select-window-9)

(add-hook 'after-init-hook #'winum-mode)

;; ace-window
;;
;; 没怎么用过，用不上
(install-package 'ace-window)
(keymap-global-set "M-o" 'ace-window)
(setq aw-keys '(?a ?o ?e ?u ?i))

(autoload #'color-outline-mode "color-outline.el" nil t)
(add-hook 'prog-mode-hook #'color-outline-mode)


;; 用不上
;;; avy
(install-package 'avy)
(with-eval-after-load 'avy
  (setq avy-background t
        avy-style 'pre))

;;; atomic-chrome
;;
;; Edit browser text with emacs.
;;
;; 从没用过
(install-package 'atomic-chrome)
(setq atomic-chrome-buffer-open-style 'frame)
(add-hook 'after-init-hook #'atomic-chrome-start-server)

;; 没用过
;;; uniline
;;
;; https://emacs-china.org/t/unline-emacs-package/28112/5?u=rua
(install-package 'uniline)

(setq meow--kbd-forward-char "<right>"
      meow--kbd-backward-char "<left>"
      meow--kbd-forward-line "<down>"
      meow--kbd-backward-line "<up>")

(with-eval-after-load "uniline"
  (keymap-set uniline-mode-map "C-c /" 'uniline-hydra-choose-body)
  (keymap-set uniline-mode-map "C-c u" 'uniline--set-brush-0)
  (keymap-set uniline-mode-map "C-c -" 'uniline--set-brush-1)
  (keymap-set uniline-mode-map "C-c +" 'uniline--set-brush-2)
  (keymap-set uniline-mode-map "C-c =" 'uniline--set-brush-3)
  (keymap-set uniline-mode-map "C-c #" 'uniline--set-brush-block)
  (keymap-set uniline-mode-map "-" nil)
  (keymap-set uniline-mode-map "+" nil)
  (keymap-set uniline-mode-map "#" nil)
  (keymap-set uniline-mode-map "=" nil))

;; 我已经不用 nix 了
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/323#issuecomment-2762214714
;; 虽然这样可以解决找不到 libgccjit 的问题，但是由于我安装了 nix，带了一个 linux
;; version 的 ld，导致 native compile 调用 libgccjit 的使用报错 unknown argument，
;; 暂时不解决这个问题，直接关掉 native compile
(defun homebrew-gcc-paths ()
  "Return GCC library paths from Homebrew installations.
Detects paths for gcc and libgccjit packages to be used in LIBRARY_PATH."
  (let* ((paths '())
         (brew-bin (or (executable-find "brew")
                       (let ((arm-path "/opt/homebrew/bin/brew")
                             (intel-path "/usr/local/bin/brew"))
                         (cond
                          ((file-exists-p arm-path) arm-path)
                          ((file-exists-p intel-path) intel-path))))))

    (when brew-bin
      ;; Get gcc paths.
      (let* ((gcc-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix gcc"))))
             (gcc-lib-current (expand-file-name "lib/gcc/current" gcc-prefix)))
        (push gcc-lib-current paths)

        ;; Find apple-darwin directory.
        (let* ((default-directory gcc-lib-current)
               (arch-dirs (file-expand-wildcards "gcc/*-apple-darwin*/*[0-9]")))
          (when arch-dirs
            (push (expand-file-name
                   (car (sort arch-dirs #'string>)))
                  paths))))

      ;; Get libgccjit paths
      (let* ((jit-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix libgccjit"))))
             (jit-lib-current (expand-file-name "lib/gcc/current" jit-prefix)))
        (push jit-lib-current paths)))

    (nreverse paths)))

(defun setup-macos-native-comp-library-paths ()
  "Set up LIBRARY_PATH for native compilation on macOS.
Includes Homebrew GCC paths and CommandLineTools SDK libraries."
  (let* ((existing-paths (split-string (or (getenv "LIBRARY_PATH") "") ":" t))
         (gcc-paths (homebrew-gcc-paths))
         (clt-paths '("/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))
         (unique-paths (delete-dups
                        (append existing-paths gcc-paths clt-paths))))

    (setenv "LIBRARY_PATH" (mapconcat #'identity unique-paths ":"))))

;; Set up library paths for native compilation on macOS.
(when (eq system-type 'darwin)
  (setup-macos-native-comp-library-paths))

;; eat
;;
;;                               ┌─────────────────────────────┐
;;                               │        Char Mode            │
;;                               │  (Full Terminal Emulation)  │
;;                               │                             │
;;                               │  • ALL keys to terminal     │
;;                               │  • No Emacs keys work       │
;;                               └─────────────────────────────┘
;;                               ▲ │           ▲               ▲
;;                               │ │           │               │
;;                       C-c M-d │ │ M-RET     │               │ C-c M-d
;;                               │ ▼           │               │
;;  ┌─────────────────────────────┐            │             ┌─────────────────────────────┐
;;  │      Semi-char Mode         │            │             │        Line Mode            │
;;  │        (Default)            │            │             │      (Comint-style)         │
;;  │                             │            │             │                             │
;;  │  All keys except:           │            │             │  • Line editing             │
;;  │  • C-\  • C-c  • C-x        │ ◀────────────────────▶   │  • History (↑↓)             │
;;  │  • C-g  • C-h  • C-M-c      │      C-c C-j / C-c C-l   │  • Tab completion           │
;;  │  • C-u  • M-x  • C-q        │            │             │  • Enter sends line         │
;;  │  • M-:  • M-!  • M-&        │            │             │                             │
;;  └─────────────────────────────┘    C-c M-d │             └─────────────────────────────┘
;;                              ▲ │            │              ▲ │
;;                      C-c C-j │ │ C-c C-e    │      C-c C-l │ │ C-c C-e
;;                              │ ▼            │              │ ▼
;;                               ┌─────────────────────────────┐
;;                               │        Emacs Mode           │
;;                               │    (Read-only buffer)       │
;;                               │                             │
;;                               │  • All Emacs keys work      │
;;                               │  • Buffer is read-only      │
;;                               │  • Free cursor movement     │
;;                               │  • Search with C-s/C-r      │
;;                               │  • Copy text only           │
;;                               └─────────────────────────────┘
(install-package 'eat)

(setq eat-term-name "xterm-256color")

(add-hook 'eshell-load-hook 'eat-eshell-mode)
(add-hook 'eshell-load-hook 'eat-eshell-visual-command-mode)

(defun my/eat-meow-setup ()
  (add-hook 'meow-normal-mode-hook 'eat-emacs-mode nil t)
  (add-hook 'meow-insert-mode-hook 'eat-char-mode nil t))

(defun my/eat-toggle-rime ()
  "切换输入法并自动切换 eat mode"
  (interactive)
  (if current-input-method
      (progn
        (deactivate-input-method)
        (eat-char-mode))
    (progn
      (activate-input-method "rime")
      (eat-line-mode))))

(with-eval-after-load "eat"
  (define-key eat-char-mode-map (kbd "C-\\") 'my/eat-toggle-rime)
  (define-key eat-line-mode-map (kbd "C-\\") 'my/eat-toggle-rime)
  (add-hook 'eat-mode-hook 'my/eat-meow-setup))
