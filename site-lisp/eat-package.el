;;; eat-package.el --- Just eat your package!        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Liu Bo

;; Author: Liu Bo <liubolovelife@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Base on luna-load-package.el

;;; Code:

(require 'pcase)

(defvar eat-all-package-daemon nil
  "If it's value is t, all package in `eat-package' will be required in dameon.")

(defconst eat--all-package-p
  (and eat-all-package-daemon (daemonp))
  "")

(defun eat-package-split-command-args (args)
  "Split args into commands and args.
If ARGS is (:command args args args :command args),
return: ((:command . (args args args)) (:command . (args)))."
  (let (ret-list arg-list command)
    (dolist (token (append args '(:finish)))
      (if (keywordp token)
          ;; Finish previous command
          (progn (if command (push (cons command (reverse arg-list))
                                   ret-list))
                 (setq arg-list nil)
                 ;; Start new command
                 (setq command token))
        (push token arg-list)))
    (reverse ret-list)))

(defun eat-package--handle-hook (arg-list package)
  "Handle hook arguments.
Each ARG in ARG-LIST is a cons (HOOK . FUNCTION).
HOOK can be either a single hook or a list of hooks.
FUNCTION can also be either a single function or a list of them.
PACKAGE is the package we are configuring."
  (let (ret-list hook-list func-list)
    (dolist (arg arg-list)
      (let ((hook (car arg))
            (func (cdr arg)))
        ;; Normalize to lists.
        (setq hook-list
              (if (symbolp hook) (list hook) hook))
        (setq func-list
              (if (or (symbolp func)
                      ;; Handle lambda correctly.
                      (functionp func))
                  (list func) func)))
      ;; Produce add-hook forms.
      (dolist (func func-list)
        ;; If FUNC is a lambda function, we can't autoload it,
        ;; Make it load the package before execution.
        (let ((func (if (not (symbolp func))
                        ;; We don't want closure.
                        `(lambda () (require ',package) (funcall ,func))
                      func)))
          (dolist (hook hook-list)
            (push `(add-hook ',hook #',func) ret-list)))))
    (reverse ret-list)))

(defun eat-package--collect-autoload (arg-list package)
  "Collect functions that needs autoload from ARG-LIST.
PACKAGE is the package we are loading.
Return a list of (autoload ...) forms."
  (let ((autoload
          (mapcan (lambda (arg)
                    (let ((command (car arg))
                          (arg-list (cdr arg)))
                      (pcase command
                        ;; ARG is either (hook . fn) or
                        ;;               ((hook ...) . fn) or
                        ;;               (hook . (fn ...))
                        (:hook (mapcan (lambda (arg)
                                         (let ((fn (cdr arg)))
                                           (if (or (symbolp fn)
                                                   ;; Handle lambda.
                                                   (functionp fn))
                                               (list fn)
                                             fn)))
                                       arg-list))
                        ;; ARG is either ".xxx" or (".xxx" . mode)
                        (:mode (mapcar (lambda (arg)
                                         (if (stringp arg)
                                             package
                                           (cdr arg)))
                                       arg-list)))))
                  arg-list)))
    (mapcar (lambda (fn)
              (if (symbolp fn)
                  `(autoload #',fn ,(symbol-name package) nil t)))
            autoload)))

(defmacro eat-package (package &rest args)
  "Like ‘use-package’.
PACKAGE is the package you are loading.
Available COMMAND:

  :init         Run right away.
  :config       Run after package loads.
  :hook         Each arguments is (HOOK . FUNC)
                HOOK and FUNC can be a symbol or a list of symbols.
  :load-path    Add load paths.
  :mode         Add (ARG . PACKAGE) to ‘auto-mode-alist’. If ARG is
                already a cons, add ARG to ‘auto-mode-alist’.
  :commands     Add autoload for this command.
  :after        Require after this package loads.
  :reqire       Require this package right now.
  :straight     Install package via straight

Each COMMAND can take zero or more ARG. Among these commands,
:hook, :commands, and :after expect literal arguments, :init,
:config expect s-expressions, which are evaluated after
expansion of the macro.

ARGS.

\(fn PACKAGE &rest [COMMAND [ARG ...]] ...)"
  (declare (indent 1))
  ;; Group commands and arguments together.
  (let* ((arg-list (eat-package-split-command-args args))
         ;; Translate commands & arguments to valid
         ;; config code.
         (body
          (mapcan
           (lambda (arg)
             (let ((command (car arg))
                   (arg-list (cdr arg)))
               (pcase command
                 (:straight `((if (listp ',@arg-list)
                                  (straight-use-package ',@arg-list)
                                (straight-use-package ',package))))
                 (:init arg-list)
                 (:config `((with-eval-after-load ',package
                              ,@arg-list)))
                 (:hook (eat-package--handle-hook arg-list package))
                 (:mode
                  ;; ARG is either ".xxx" or (".xxx" . mode)
                  (mapcar
                   (lambda (arg)
                     (let ((pattern (if (consp arg) (car arg) arg))
                           (mode-fn (if (consp arg) (cdr arg) package)))
                       `(add-to-list 'auto-mode-alist
                                     ',(cons pattern mode-fn))))
                   arg-list))
                 (:commands
                  (mapcar (lambda (cmd)
                            `(autoload ',cmd ,(symbol-name package) nil t))
                          arg-list))
                 (:after
                  (mapcar (lambda (pkg)
                            `(with-eval-after-load ',pkg
                               (require ',package)))
                          arg-list)))))
           arg-list))
         (autoload-list (eat-package--collect-autoload arg-list package))
         ;; Must :require explicitly if you want to require this package.
         (require-p (let ((commands (mapcar #'car arg-list)))
                      (or (memq :require commands)))))
    `(condition-case err
         (progn
           ,@autoload-list
           ,@body
           (if eat--all-package-p
               (require ',package)
             ,(when require-p `(require ',package))))
       ((debug error) (warn "Error when loading %s: %s" ',package
                            (error-message-string err))))))

(provide 'eat-package)
;;; eat-package.el ends here
