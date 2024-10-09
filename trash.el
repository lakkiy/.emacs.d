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

;;; dirvish
(install-package 'dirvish)

(setq dirvish-attributes '(vc-state subtree-state nerd-icons)
      dirvish-header-line-height 20
      dirvish-mode-line-height 20)

(keymap-global-set "<f1>" #'dirvish-side)

(with-eval-after-load 'dirvish
  ;; (dirvish-side-follow-mode) ;; FIXME
  (define-key dirvish-mode-map (kbd "TAB") #'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "<tab>") #'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "a") #'dirvish-quick-access)
  (define-key dirvish-mode-map (kbd "f") #'dirvish-file-info-menu)
  (define-key dirvish-mode-map (kbd "y") #'dirvish-yank-menu)
  (define-key dirvish-mode-map (kbd "N") #'dirvish-narrow)
  (define-key dirvish-mode-map (kbd "H") #'dirvish-history-jump)
  (define-key dirvish-mode-map (kbd "s") #'dirvish-quicksort)
  (define-key dirvish-mode-map (kbd "v") #'dirvish-vc-menu)
  (define-key dirvish-mode-map (kbd "M-f") #'dirvish-history-go-forward)
  (define-key dirvish-mode-map (kbd "M-b") #'dirvish-history-go-backward)
  (define-key dirvish-mode-map (kbd "M-l") #'dirvish-ls-switches)
  (define-key dirvish-mode-map (kbd "M-m") #'dirvish-mark-menu)
  (define-key dirvish-mode-map (kbd "M-t") #'dirvish-layout-toggle)
  (define-key dirvish-mode-map (kbd "M-s") #'dirvish-setup-menu)
  (define-key dirvish-mode-map (kbd "M-e") #'dirvish-emerge-menu)
  (define-key dirvish-mode-map (kbd "M-j") #'dirvish-fd-jump)
  (define-key dirvish-mode-map (kbd "<mouse-1>") #'dirvish-subtree-toggle-or-open)
  (define-key dirvish-mode-map (kbd "<mouse-2>") #'dired-mouse-find-file-other-window)
  (define-key dirvish-mode-map (kbd "<mouse-3>") #'dired-mouse-find-file))

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

;; minions
(install-package 'minions)
(add-hook 'after-init-hook 'minions-mode)


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
(defun my/hl-line-setup ()
  "Disable `hl-line-mode' if region is active."
  (when (and (bound-and-true-p hl-line-mode)
             (region-active-p))
    (hl-line-unhighlight)))
(with-eval-after-load 'hl-line
  (add-hook 'post-command-hook #'my/hl-line-setup))
(when (display-graphic-p)
  (add-hook 'prog-mode-hook #'hl-line-mode))
;; FIXME 打开两个一样的窗口并且都用 hl line 开启， scrool 那个 inactive 的高亮位
;; 置也会变


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
