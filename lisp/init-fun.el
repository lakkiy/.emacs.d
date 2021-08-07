;;; -*- lexical-binding: t -*-

(eat-package emacs-application-framework
  :straight (emacs-application-framework
             :type git
             :host github
             :repo "manateelazycat/emacs-application-framework"
             :files ("*.el" "*.py" "core" "app"))
  :init
  (eat-package s :straight t)
  (eat-package ctable :straight t)
  (eat-package epc :straight t)
  (eat-package deferred :straight t)
  (setq eaf-browser-continue-where-left-off t)
  :config
  (setq eaf-browser-enable-adblocker "true"))

(eat-package simple-call-tree :straight t)

(eat-package chess
  :straight t
  :init
  (setq chess-default-display '(chess-plain chess-ics1 chess-images)))

(eat-package sdcv
  :straight (sdcv :type git :host github :repo "manateelazycat/sdcv")
  :commands
  sdcv-search-pointer
  sdcv-search-pointer+
  sdcv-search-input
  sdcv-search-input+
  :init
  ;; TODO sdcv-dwim
  (setq sdcv-dictionary-data-dir (file-truename "~/.sdcv-dict")
        sdcv-dictionary-simple-list
        '("懒虫简明英汉词典"
          "懒虫简明汉英词典"
          "KDic11万英汉词典")
        sdcv-dictionary-complete-list
        '("懒虫简明英汉词典"
          "英汉汉英专业词典"
          "XDICT英汉辞典"
          "stardict1.3英汉辞典"
          "WordNet"
          "XDICT汉英辞典"
          "懒虫简明汉英词典"
          "新世纪英汉科技大词典"
          "KDic11万英汉词典"
          "朗道汉英字典5.0"
          "CDICT5英汉辞典"
          "新世纪汉英科技大词典"
          "牛津英汉双解美化版"
          "21世纪双语科技词典"
          "quick_eng-zh_CN")))

(provide 'init-fun)
