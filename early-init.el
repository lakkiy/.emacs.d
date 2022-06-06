(setq package-enable-at-startup nil
      ;; Resizing the Emacs frame can be a terribly expensive part of changing the
      ;; font. By inhibiting this, we easily halve startup times with fonts that are
      ;; larger than the system default.
      frame-inhibit-implied-resize t
      ;; After startup `gcmh' will reset this.
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      ;; Faster to disable these here (before they've been initialized)
      default-frame-alist '((scroll-bar-mode . 0)
                            (vertical-scroll-bars . nil)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)))
