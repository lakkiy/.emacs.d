;; Speed up startup
(setq auto-mode-case-fold nil)

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (add-function :after after-focus-change-function
                          (lambda ()
                            (unless (frame-focus-state)
                              (garbage-collect))))

            ;; Recover GC values after startup
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

(add-hook 'after-init-hook
          (lambda ()
            (save-excursion
              (switch-to-buffer "*scratch*")
              (goto-char (point-min))
              (insert ";;
;;            :h-                                  Nhy`
;;           -mh.                           h.    `Ndho
;;           hmh+                          oNm.   oNdhh
;;          `Nmhd`                        /NNmd  /NNhhd
;;          -NNhhy                      `hMNmmm`+NNdhhh
;;          .NNmhhs              ```....`..-:/./mNdhhh+
;;           mNNdhhh-     `.-::///+++////++//:--.`-/sd`
;;           oNNNdhhdo..://++//++++++/+++//++///++/-.`
;;      y.   `mNNNmhhhdy+/++++//+/////++//+++///++////-` `/oos:
;; .    Nmy:  :NNNNmhhhhdy+/++/+++///:.....--:////+++///:.`:s+
;; h-   dNmNmy oNNNNNdhhhhy:/+/+++/-         ---:/+++//++//.`
;; hd+` -NNNy`./dNNNNNhhhh+-://///   -+ooo:`  ::-:+////++///:`
;; /Nmhs+oss-:++/dNNNmhho:--::///   /mmmmmmo  ../-///++///////.
;;  oNNdhhhhhhhs//osso/:---:::///   /myyyyso  ..o+-//////////:/.
;;   /mNNNmdhhhh/://+///::://////     -:::- ..+sy+:////////::/:/.
;;     /hNNNdhhs--:/+++////++/////.      ..-/yhhs-/////////::/::/`
;;       .ooo+/-::::/+///////++++//-/ossyyhhhhs/:///////:::/::::/:
;;       -///:::::::////++///+++/////:/+ooo+/::///////.::://::---+`
;;       /////+//++++/////+////-..//////////::-:::--`.:///:---:::/:
;;       //+++//++++++////+++///::--                 .::::-------::
;;       :/++++///////////++++//////.                -:/:----::../-
;;       -/++++//++///+//////////////               .::::---:::-.+`
;;       `////////////////////////////:.            --::-----...-/
;;        -///://////////////////////::::-..      :-:-:-..-::.`.+`
;;         :/://///:///::://::://::::::/:::::::-:---::-.-....``/mm`
;;           ::::://::://::::::::::::::----------..-:....`.../Nmhd+o/
;;            -/:::-:::::---://:-::-::::----::---.-.......`-/oNN   ``
;;           s-`::--:::------:////----:---.-:::...-.....`./:
;;          yMNy.`::-.--::..-dmmhhhs-..-.-.......`.....-/:`
;;         oMNNNh. `-::--...:NNNdhhh/.--.`..``.......:/-
;;        :dy+:`      .-::-..NNNhhd+``..`...````.-::-`
;;                        .-:mNdhh:.......--::::-`
;;                          yNh/..------..`
;;
")
              (insert ";; \t\t\t\t\tE M A C S\n")
              (insert (format ";; %s\n" (make-string 58 ?-)))
              (insert (format ";; Welcome to GNU Emacs %s. "
                              emacs-version))
              (insert (format "Today is %s .\n"
                              (format-time-string "%A %Y.%-m.%-d")))
              (insert ";;\n")
              (goto-char (point-max)))))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and (file-exists-p custom-file)
           (file-readable-p custom-file))
  (load custom-file :no-error :no-message))

;; Shut up!
(defun display-startup-echo-area-message()
  (message nil))

(defvar +foo-p nil
  "TODO Find a name.")

(let ((file-name-handler-alist nil))
  (require 'init-straight)
  (require 'init-basic)
  (require 'init-builtin)
  (require 'init-theme)
  (require 'init-meow)
  (require 'init-rime)
  (require 'init-minibuffer)
  (require 'init-modeline)
  (require 'init-window)
  (when (and +foo-p (display-graphic-p))
    (require 'init-foo))
  (run-with-idle-timer
   1 nil
   #'(lambda ()
       (require 'init-edit)
       (require 'init-completion)
       (require 'init-dev)
       (require 'init-telega)
       (require 'init-git)
       (require 'init-org)
       (require 'init-dired)
       (require 'init-ibuffer)
       (require 'init-spcfile)
       (require 'init-mole)
       (require 'init-mail)
       (unless window-system
         (require 'init-xterm))
       (require 'init-fun)
       (add-hook 'after-init-hook 'server-mode))))
