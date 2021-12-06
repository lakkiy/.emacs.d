;;; -*- lexical-binding: t -*-

;; install pyqt5 with your package manager(not pip)
(eat-package popweb
  :straight (popweb
             :type git
             :host github
             :repo "manateelazycat/popweb"
             :files ("*.el" "*.py" "*.js" "*.html" "*.css" "extension")))

(eat-package rainbow-mode
  :straight t
  :commands rainbow-mode)

(eat-package secret-mode
  :straight (secret-mode :type git :host github :repo  "bkaestner/secret-mode.el"))

(eat-package screenshot
  :straight (screenshot :type git :host github :repo "tecosaur/screenshot"))

(eat-package find-orphan
  :straight (find-orphan
             :type git
             :host github
             :repo "manateelazycat/find-orphan")
  :commands
  find-orphan-function-in-buffer
  find-orphan-function-in-directory)

(provide 'init-fun)
