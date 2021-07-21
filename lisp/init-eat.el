;;; -*- lexical-binding: t -*-

(defgroup eat nil
  "Eat Emacs customization"
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/404cn/eatemacs"))

(defcustom eat-system-themes '((light . spacemacs-light)
                               (dark  . spacemacs-dark))
  "List of themes related the system appearance. It's only available on macOS."
  :group 'eat
  :type '(alist :key-type (symbol :tag "Appearance")
                :value-type (symbol :tag "Theme")))

(provide 'init-eat)
