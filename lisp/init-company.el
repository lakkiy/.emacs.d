;;; -*- lexical-binding: t -*-
;;
;; - use C-p/C-n to select prev/next
;; - use tab to do complete
;; - free ret, popup will no longer interrupt typing
;; - when disable company, use =completion-at-point= to do it manually

(install-package 'company)

(add-hook 'after-init-hook 'global-company-mode)

;; Basic config.
(defun my-company-capf--candidates (func &rest args)
  "Try default completion styles."
  (let ((completion-styles '(basic partial-completion)))
    (apply func args)))
(advice-add 'company-capf--candidates :around 'my-company-capf--candidates)

(setq company-minimum-prefix-length 2
      company-idle-delay 0.1 ;; time for snippet expand
      company-require-match nil)

;;; Frontend
;;
;; don't need preview frontends with copilot
;; use C-h to show doc(maybe use eldoc-box) instead echo- frontend
;; so only config tooltip frontend
(setq company-frontends '(company-pseudo-tooltip-frontend)
      company-tooltip-align-annotations t
      ;; no matter if a tooltip is shown above or below point, the
      ;; candidates are always listed starting near point.
      company-tooltip-width-grow-only t
      ;; icon in front of a candidate, make it looks like Atom
      company-format-margin-function #'company-text-icons-margin
      company-text-icons-add-background t)

;;; Backend
;;
;; Do not use company-capf with company-yasnippet, it will cause
;; completions list messed up.
(setq company-backends '(company-capf
                         company-files
                         (;;searching for completion candidates inside the contents of the open buffer(s)
                          company-dabbrev-code
                          ;; provides completions from programming language keywords
                          company-keywords)
                         company-dabbrev)
      company-files-exclusions '(".git/" ".DS_Store")
      ;; search from the buffers with the same majar mode
      company-dabbrev-other-buffers t
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil
      company-dabbrev-code-ignore-case nil
      company-dabbrev-code-everywhere t)

;;; Keybinding
(with-eval-after-load "company"
  (define-key company-active-map [tab] #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "C-s") #'company-filter-candidates)
  ;; Free SPC and RET, popup will no longer interrupt typing.
  ;; use tab to do complete and free ret
  (define-key company-active-map [escape] nil)
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "SPC") nil))

;;; Manually complete
;;
;; (setq company-idle-delay nil)
;; (with-eval-after-load 'company
;;   (keymap-substitute company-mode-map #'completion-at-point #'company-complete)
;;   (keymap-set company-mode-map "C-M-i" #'company-complete))

;;; yasnippet
(install-package 'yasnippet)

(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'conf-mode-hook #'yas-minor-mode)

(with-eval-after-load 'yasnippet
  (let ((inhibit-message t))
    (yas-reload-all)))

;;; copilot
(install-package 'copilot "https://github.com/zerolfx/copilot.el")

;; 由于 `lisp-indent-offset' 的默认值是 nil，在编辑 elisp 时每敲一个字
;; 符都会跳出一个 warning，将其默认值设置为 t 以永不显示这个 warning
(setq-default copilot--indent-warning-printed-p t
              copilot-indent-offset-warning-disable t)

(add-hook 'prog-mode-hook 'copilot-mode)

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

(provide 'init-company)
