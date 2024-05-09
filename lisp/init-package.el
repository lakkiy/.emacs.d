;;; -*- lexical-binding: t -*-

(setq package-archives
      '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; To prevent initializing twice
(setq package-enable-at-startup nil)
(package-initialize)

(defun install-package (pkg &optional url)
  (unless (package-installed-p pkg)
    (if url
        (package-vc-install url)
      (unless (package-installed-p pkg)
        (package-refresh-contents))
      (package-install pkg))))


;; https://github.com/PythonNut/quark-emacs/blob/dev/modules/config-package.el
(defvar idle-jobs nil
  "Symbols which need to be autoloaded.")

(defvar idle-job-timer (run-with-idle-timer 0.1 t 'idle-job-run-next))

(defun idle-job-run-next ()
  "Load symbols from `idle-require-symbols' until input occurs."
  (while (and idle-jobs
              (not (input-pending-p)))
    (cl-letf* ((old-load (symbol-function #'load))
               ((symbol-function #'load)
                (lambda (file &optional noerror _nomessage &rest args)
                  (apply old-load
                         file
                         noerror
                         (not (eq debug-on-error 'startup))
                         args))))
      (with-demoted-errors "Idle job error: %s"
        (funcall (pop idle-jobs))))))

(defun idle-job-add-require (sym &optional append)
  (cl-letf ((fun (lambda ()
                   (let ((start-time (current-time))
                         (verbose debug-on-error))
                     (unless (require sym nil t)
                       (message "failed to load %s" sym))

                     (when verbose
                       (message "%.3f loaded %s"
                                (float-time (time-subtract
                                             (current-time)
                                             start-time))
                                sym))))))
    (if append
        (setq idle-jobs (append idle-jobs (list fun)))
      (push fun idle-jobs))))

(defun idle-job-add-function (sym &optional append)
  (cl-letf ((fun (lambda ()
                   (let ((start-time (current-time))
                         (verbose debug-on-error))
                     (funcall sym)
                     (when verbose
                       (message "%.3f ran %S"
                                (float-time (time-subtract
                                             (current-time)
                                             start-time))
                                sym))))))
    (if append
        (setq idle-jobs (append idle-jobs (list fun)))
      (push fun idle-jobs))))

(provide 'init-package)
