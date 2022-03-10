(let ((file-name-handler-alist nil))
  ;; 1. must work on emacs -Q
  (require 'init-straight)
  ;; 2. some funcs
  (require 'init-my)
  ;; 3. must work on normal
  (require 'init-dog)
  (require 'init-edit)
  (require 'init-completion)
  (require 'init-dev)
  ;; 4. 鸡肋
  (when (and +icons-p (display-graphic-p))
    (require 'init-icons))
  ;; 5. theme modeline
  (require 'init-ui)
  ;; 6. emacs app，telega，magit
  (require 'init-app)
  ;; 7. modes
  (require 'init-mode)
  (require 'init-org)
  ;; 8. lib like all-the-icons
  (require 'init-lib)
  ;; 9. just straight
  (require 'init-mole)
  (unless window-system
    (require 'init-xterm)))
