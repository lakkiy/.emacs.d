;;; -*- lexical-binding: t -*-

(straight-use-package 'olivetti)
(straight-use-package 'rainbow-mode)
(straight-use-package 'docstr)
(straight-use-package 'parrot)
(straight-use-package '(vundo :type git :host github :repo "casouri/vundo"))
(straight-use-package 'leetcode)
(straight-use-package 'restclient)
(straight-use-package '(insert-translated-name :type git :host github :repo "manateelazycat/insert-translated-name"))
(straight-use-package '(company-english-helper :type git :host github :repo "manateelazycat/company-english-helper"))
(straight-use-package '(emacs-calfw :type git :host github :repo "kiwanami/emacs-calfw"))
(straight-use-package 'insert-char-preview)
(straight-use-package 'major-mode-hydra)
(straight-use-package 'projectile)

;; projectile
(setq
 projectile-use-git-grep t
 projectile-indexing-method 'alien
 projectile-globally-ignored-files '("TAGS", ".DS_Store")
 projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".swp" ".so" ".a"))

(add-hook 'after-init-hook 'projectile-mode)

(with-eval-after-load "projectile"
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; major-mode-hydra
;; TODO maybe just need pretty-hydra
(global-set-key (kbd "<f6>") #'major-mode-hydra)
(autoload #'major-mode-hydra "major-mode-hydra" nil t)


;; insert-char-preview
(autoload 'insert-char-preview "insert-char-preview" nil t)

;; emacs-calfw
(setq cfw:org-overwrite-default-keybinding t)

(autoload 'cfw:open-calendar-buffer "calfw" nil t)
(autoload 'cfw:open-org-calendar "calfw-org" nil t)

(with-eval-after-load "calfw"
  ;; SPC-SPC is used in Motion mode to run M-X
  (define-key cfw:calendar-mode-map (kbd "RET") 'cfw:show-details-command))

;; company-english-helper
(autoload 'toggle-company-english-helper "company-english-helper" nil t)

;; insert-translated-name
(autoload 'insert-translated-name-insert "insert-translated-name" nil t)

;; olivetti
(autoload 'olivetti-mode "olivetti" nil t)

;; rainbow-mode
(autoload 'rainbow-mode "rainbow-mode")

;; docstr
(add-hook 'prog-mode-hook (lambda () (docstr-mode 1)))


;; TODO parrot
;; add to modeline

;; vundo
;; TODO add to normal config file
(autoload 'vundo "vundo" nil t)

;; leetcode
;; TODO add to normal config
(setq
 leetcode-prefer-language "golang"
 leetcode-prefer-sql "mysql"
 leetcode-save-solutions t
 leetcode-directory "~/Dropbox/leetcode")

(autoload 'leetcode "leetcode" nil t)

;; restclient
;; TODO find other packages like this or add to to normal config
(autoload 'restclient-mode "restclient" nil t)

(provide 'init-fun)
