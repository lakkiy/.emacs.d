;;; -*- lexical-binding: t -*-


;;; go-translate
;;
;; Write to ~/.authinfo
;; machine api.deepl.com login auth-key password ****
;; machine api.openai.com login apikey password ****
(install-package 'go-translate)

(setq gt-langs '(en zh)
      gt-chatgpt-model "gpt-3.5-turbo-16k")

(with-eval-after-load 'go-translate
  (setq my/gt-engine
        (append (when (auth-source-search :host "api.openai-sb.com" :user "apikey")
                  (setq gt-chatgpt-host "https://api.openai-sb.com")
                  (list (gt-chatgpt-engine)))
                (when (auth-source-search :host "api.deepl.com" :user "auth-key")
                  (list (gt-deepl-engine)))
                (list (gt-google-engine) (gt-bing-engine))))
  ;; NOTE youdao 不支持分段翻译
  (setq gt-default-translator (gt-translator :engines my/gt-engine :render  (gt-buffer-render))))

(defun my/gt-immersive-translate ()
  "Overlay render gt-do-translate.
Default to translate buffer, or select region."
  (interactive)
  (require 'go-translate)
  (gt-start (gt-translator
             :taker (gt-taker :text 'buffer)
             :engines (car my/gt-engine)
             :render (gt-overlay-render))))

(add-hook 'gt-buffer-render-init-hook #'visual-fill-column-mode)


;;; fanyi
(install-package 'fanyi)

(defun my/translate ()
  (interactive)
  (if (use-region-p)
      (gt-do-translate)
    (fanyi-dwim2)))
(keymap-global-set "C-c y" #'my/translate)

(defun my/context-translate (menu click)
  "My context MENU to translate text."
  (define-key-after menu [dictionary-lookup]
    '(menu-item "Translate" my/translate))
  menu)
(add-hook 'context-menu-functions #'my/context-translate)


;;; init-translator.el ends here
(provide 'init-translator)
