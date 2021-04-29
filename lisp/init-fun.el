;;; -*- lexical-binding: t -*-


(straight-use-package 'ibuffer-vc)


(straight-use-package 'olivetti)
(autoload 'olivetti-mode "olivetti" nil t)


(straight-use-package 'rainbow-mode)
(autoload 'rainbow-mode "rainbow-mode")


(straight-use-package 'docstr)
(add-hook 'prog-mode-hook (lambda () (docstr-mode 1)))


;; TODO
(straight-use-package 'parrot)

(leaf eaf
  :doc "monkeytype in company (, don't forget run npm install"
  :straight
  (eaf :type git
       :host github
       :repo "manateelazycat/emacs-application-framework"
       :files ("*"))
  :init
  (leaf epc :straight t :leaf-defer t)
  (leaf ctable :straight t :leaf-defer t)
  (leaf deferred :straight t :leaf-defer t)
  (leaf s :straight t :leaf-defer t)
  :commands
  (eaf-open-browser eaf-open eaf-open-bookmark)
  :config
  (require 'eaf-org)
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-setq eaf-browser-enable-autofill "true"))


(straight-use-package '(vundo :type git :host github :repo "casouri/vundo"))
(autoload 'vundo "vundo" nil t)


(straight-use-package 'vterm)
(autoload 'vterm "vterm" nil t)


(straight-use-package 'leetcode)

(setq
 leetcode-prefer-language "golang"
 leetcode-prefer-sql "mysql"
 leetcode-save-solutions t
 leetcode-directory "~/Dropbox/leetcode")

(autoload 'leetcode "leetcode" nil t)


(straight-use-package 'restclient)
(autoload 'restclient-mode "restclient" nil t)

(provide 'init-fun)
