;;; Iniline refined diff

;; Copyright (C) 2022

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
;; https://www.reddit.com/r/emacs/comments/tun9qi/an_experiment_to_make_diffs_easier_to_read/

;;
;; TODO:
;;
;;



;;; Code:


(setq inline-diff-refine-mode nil)

(defun inline-diff-refine-mode ()
  (interactive)

  (setq inline-diff-refine-mode (not inline-diff-refine-mode))

  (if inline-diff-refine-mode
      (progn
        (advice-add 'vc-do-command :after  'my-vc-do-command)
        (advice-add 'vc-diff :before  'my-vc-diff-catch-directory))

    (advice-remove 'vc-do-command 'my-vc-do-command)
    (advice-remove 'vc-diff 'my-vc-diff-catch-directory))

  (message "Turned inline diff refine %s"
           (if inline-diff-refine-mode
               "on"
             "off")))





(defun my-vc-diff-catch-directory (&optional HISTORIC NOT-URGENT)
  (setq my-inline-diff-directory default-directory))


(defun my-vc-do-command (buffer okstatus command file-or-list &rest flags)
  (setq my-inline-diff-buffer (current-buffer))
  (let ((full-command (concat
                       command
                       " " (vc-delistify flags)
                       " " (vc-delistify (mapcar (lambda (f) (file-relative-name (expand-file-name f)))
		                                         (if (listp file-or-list) file-or-list (list file-or-list)))))))
    (if (string-match "diff-index" full-command)
        (my-inline-diff-wait-for-diff
         (replace-match "\\& --word-diff --word-diff-regex=. " nil nil full-command)
         (buffer-modified-tick)))))


(defun my-inline-diff-wait-for-diff (command tick)
  (run-with-timer
   0.1 nil
   (lambda (command tick)
     (if (eq tick (buffer-modified-tick))
         (my-inline-diff-refine-diff command)

       (my-inline-diff-wait-for-diff command (buffer-modified-tick))))
   command tick))


(defun my-inline-diff-refine-diff (command)
  (let* ((add_re "{\\+\\(.+?\\)\\+}")
         (del_re "\\[-\\(.+?\\)-\\]")
         (change_re (concat add_re "\\|" del_re))
         changes
         (make-evaporating-overlay
          (lambda (start end &optional props)
            (let ((o (make-overlay start end)))
              (overlay-put o 'evaporate t)
              (dolist (prop props)
                (overlay-put o (car prop) (cdr prop)))
              o))))

    (with-current-buffer (get-buffer-create "*diffbuff*")
      (erase-buffer)
      (if (= 2 (let ((default-directory my-inline-diff-directory))
                 (call-process-shell-command
                  command nil (current-buffer) nil)))
          (error (buffer-string)))

      (goto-char (point-min))

      (while (re-search-forward change_re nil t)
        (unless (and (eq (match-beginning 0) (line-beginning-position))
                     (eq (match-end 0) (line-end-position)))
          (push  (buffer-substring (line-beginning-position)
                                   (line-end-position))
                 changes))
        (forward-line 1)))

    (with-current-buffer my-inline-diff-buffer
      (save-excursion
        (dolist (change changes)
          (goto-char (point-min))
          (when (re-search-forward
                 (concat "^\\+"
                         (regexp-quote
                          (replace-regexp-in-string
                           add_re "\\1"
                           (replace-regexp-in-string
                            del_re ""
                            change))))
                 nil t)

            (beginning-of-line)
            (funcall make-evaporating-overlay (point) (1+ (line-end-position))
                     (list (cons 'face (cons 'background-color (frame-parameter nil 'background-color)))))
            (forward-char 1)
            (let ((pos 0)
                  newpos)
              (while (setq newpos (string-match change_re change pos))
                (forward-char (- newpos pos))
                (setq pos (+ newpos (length (match-string 0 change))))
                (if (eq (aref (match-string 0 change) 0) ?\[)
                    (overlay-put (funcall make-evaporating-overlay (point) (1+ (point)))
                                 'before-string
                                 (propertize (match-string 2 change)
                                             'face 'diff-removed))

                  (funcall make-evaporating-overlay
                           (point)
                           (+ (point) (length (match-string 1 change)))

                           '((face . diff-added)))
                  (decf pos (length (match-string 1 change))))))

            (let ((deleted (regexp-quote
                            (replace-regexp-in-string
                             del_re "\\1"
                             (replace-regexp-in-string
                              add_re ""
                              change)))))
              (unless (equal deleted "")
                (goto-char (point-min))
                (if (re-search-forward (concat "^-" deleted) nil t)
                    (overlay-put (funcall make-evaporating-overlay
                                          (line-beginning-position)
                                          (1+ (line-end-position)))
                                 'invisible t))))))))))
