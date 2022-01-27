(let ((file-name-handler-alist nil))
  ;; 1. 必要，emacs -Q 也能用
  (require 'init-straight)
  ;; 2. 自己写的函数之类的
  (require 'init-my)
  ;; 3. 正常使用必须加载
  (require 'init-dog)
  (require 'init-edit)
  (require 'init-completion)
  (require 'init-dev)
  ;; 4. 可有可无，美化，去掉也没关系
  ;; TODO 把 all-the-icons 拆成单独的库
  (when (and +icons-p (display-graphic-p))
    (require 'init-icons))
  ;; 5. 外观，主题，modeline
  (require 'init-ui)
  ;; 6. emacs app，telega，magit
  (require 'init-app)
  ;; 7. 各种 mode
  (require 'init-mode)
  (require 'init-org)
  ;; 8. TODO 库 all-the-icons
  (require 'init-lib)
  ;; 9. 直接 straight 然后不用管的
  (require 'init-mole)
  (unless window-system
    (require 'init-xterm)))
