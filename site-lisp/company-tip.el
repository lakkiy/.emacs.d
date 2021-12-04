;;; company-tip.el --- Popup documentation for completion candidates  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: company popup documentation tip
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3") (s "1.9.0") (cl-lib "0.5") (dash "2.10.0") (company "0.8.9"))

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; When idling on a completion candidate the documentation for the
;; candidate will pop up after `company-tip-delay' seconds.
;;
;; The documentation will first try to show on the right side.  If there
;; is not enough space on the right side, it will check the right, left,
;; top and bottom side, and show on the one with the most space.  The
;; documentation string may be truncated if it is too long.
;; --------------------------------------

;;; Usage:
;;
;; put (company-tip-mode) in your init.el to activate
;; `company-tip-mode'.
;;
;; You can adjust the time it takes for the documentation to pop up by
;; changing `company-tip-delay'
;; --------------------------------------

;;; Code:
(require 'company)
(require 'cl-lib)
(require 's)
(require 'dash)

(defgroup company-tip nil
  "Documentation popups for `company-mode'"
  :group 'company)

(defcustom company-tip-delay 0.5
  "Delay, in seconds, before the tip popup appears.

If set to nil the popup won't automatically appear, but can still
be triggered manually using `company-tip-manual-begin'."
  :type '(choice (number :tag "Delay in seconds")
                 (const :tag "Don't popup help automatically" nil))
  :group 'company-tip)

(defcustom company-tip-min-cols-if-show-on-right 5
  "The minimum number of columns if the doc is to be shown on the right side.

The doc will be preferred showing on the right side.  But if the actual number
of columns is smaller than this variable, the doc will look to the other sides."
  :type 'integer
  :group 'company-tip)

(defface company-tip-background
  '((t :background "#222222"))
  "Background color of the documentation.
Only the `background' is used in this face."
  :group 'company-tip)

(defvar company-tip-overlays nil
  "Tip overlays.")

(defvar-local company-tip--timer nil
  "Quickhelp idle timer.")

(defun company-tip-frontend (command)
  "`company-mode' front-end showing documentation in a popup."
  (pcase command
    (`pre-command
     (company-tip--hide)
     (company-tip--cancel-timer))
    (`post-command
     (company-tip--hide)
     (when company-pseudo-tooltip-overlay
       (company-tip--set-timer)))))

(defun company-tip--skip-footers-backwards ()
  "Skip backwards over footers and blank lines."
  (beginning-of-line)
  (while (and (not (= (point-at-eol) (point-min)))
              (or
               ;; [back] appears at the end of the help elisp help buffer
               (looking-at-p "\\[back\\]")
               ;; [source] cider's help buffer contains a link to source
               (looking-at-p "\\[source\\]")
               (looking-at-p "^\\s-*$")))
    (forward-line -1)))

(defun company-tip--docstring-from-buffer (start)
  "Fetch docstring from START."
  (goto-char (point-max))
  (company-tip--skip-footers-backwards)
  ;; (buffer-substring-no-properties start (point-at-eol))
  (buffer-substring start (point-at-eol)))

(defun company-tip--completing-read (prompt candidates &rest rest)
  "`cider', and probably other libraries, prompt the user to
resolve ambiguous documentation requests.  Instead of failing we
just grab the first candidate and press forward."
  (car candidates))

(defun company-tip--fetch-docstring (backend)
  "Fetch docstring from BACKEND.
Via either `quickhelp-string' command or `doc-buffer' command."
  (or
   (let ((doc-str (company-call-backend 'quickhelp-string backend)))
     (when (and (stringp doc-str) (not (string-blank-p doc-str)))
       (with-temp-buffer
         (insert doc-str)
         (company-tip--docstring-from-buffer (point-min)))))
   (let ((doc (company-call-backend 'doc-buffer backend)))
     (when doc
       ;; The company backend can either return a buffer with the doc or a
       ;; cons containing the doc buffer and a position at which to start
       ;; reading.
       (let ((doc-buffer (if (consp doc) (car doc) doc))
             (doc-begin (when (consp doc) (cdr doc))))
         (with-current-buffer doc-buffer
           (let ((doc-str (company-tip--docstring-from-buffer (or doc-begin (point-min)))))
             (and doc-str (not (string-blank-p doc-str)) doc-str))))))))

(defun company-tip--manual-begin ()
  "Manually trigger the `company-tip' popup for the currently active `company' completion candidate."
  (interactive)
  ;; This might seem a bit roundabout, but when attempting to call
  ;; `company-tip--show' in a more direct manner it may trigger a
  ;; redisplay of company's list of completion candidates which looked
  ;; quite weird.
  (let ((company-tip-delay 0.01))
    (company-tip--set-timer)))

(defun company-tip--hide ()
  "Hide the current tip tip."
  (when (> (length company-tip-overlays) 0)
    (seq-do 'delete-overlay company-tip-overlays)
    (setq company-tip-overlays nil)))

(defun company-tip--wrapped-line (line line-width)
  "Wrap a line of text LINE to max width LINE-WIDTH."
  (let ((trimmed (string-trim-right line)))
    (cond ((string-empty-p trimmed) "")
          ((< (length trimmed) (- line-width 2)) trimmed)
          (t (s-word-wrap (- line-width 2) trimmed)))))

(defun company-tip--padding (line len)
  "Add trailing whitespaces to string LINE to reach length LEN.
Then add one whitespace to begin and end of it.

There might be words longer than LINE-WIDTH, in which case they have to be
truncated."
  (let ((line (if (> (string-width line) len) (concat " " (substring line 0 len) " ")
                (concat " " line (make-string (- len (string-width line)) ?\s) " "))))
    (add-face-text-property 0 (length line) (list :background (face-background 'company-tooltip nil t)) t line)
    ;; (add-face-text-property 0 (length line) (list :background (face-background 'company-tip-background nil t)) t line)
    line))

(defun company-tip--format-string (string line-width)
  "Wrap STRING to max width LINE-WIDTH, and truncated at max height HEIGHT."
  (let* ((lines (--> string
                     (s-lines it)
                     (-map (lambda (line) (company-tip--wrapped-line line line-width)) it)
                     (string-join it "\n")
                     (s-lines it)
                     ))
         (doc-tip-line-width (min line-width (-max (-map (lambda (line) (length line)) lines)))))
    (-map (lambda (line) (company-tip--padding line doc-tip-line-width)) lines)))

(defun company-tip--pos-after-lines (start n)
  "Get the position at START + forward N lines."
  (save-excursion (goto-char start)
                  (forward-line n)
                  (point)))

(defun company-tip--merge-docstrings (old-strings doc-strings position)
  "Concatenate DOC-STRINGS to OLD-STRINGS.

The 3rd arg POSITION, indicates at which side the doc will be rendered."
  (let* ((ov company-pseudo-tooltip-overlay)
         (tooltip-width (overlay-get ov 'company-width))
         (company-column (overlay-get ov 'company-column))
         (horizontal-span (+ (company--window-width) (window-hscroll)))
         (tooltip-column (min (+ 1 (- horizontal-span tooltip-width)) company-column))
         (doc-strings-width (--> doc-strings
                                 (mapcar 'length it)
                                 (seq-max it)))
         (index-start (pcase position
                        ('right
                         (+ tooltip-column tooltip-width))
                        ('left
                         (- tooltip-column doc-strings-width 2))
                        (_
                         (cond
                          ((>= (- horizontal-span (- tooltip-column 1)) doc-strings-width) (- tooltip-column 1))
                          (t (- horizontal-span doc-strings-width))
                          ;; (1+ (window-hscroll))
                          )))))
    (company-tip--merge-lines old-strings doc-strings index-start)))

(defun company-tip--merge-lines (lines1 lines2 start)
  "Concatenate LINES1 and LINES2.

Replacing a substring starting from START of each line in LINES1 with the
corresponding line in LINES2."
  (let* ((index-start start)
         (result-strings nil))
    (dotimes (i
              (max (length lines1) (length lines2))
              (nreverse result-strings))
      (!cons
       (let* ((line1 (nth i lines1))
              (line2 (nth i lines2))
              (index-end (+ index-start (length line2))))
         (cond
          ((not line2)
           line1)
          ((not line1)
           (concat (truncate-string-to-width "" index-start nil ?\s)
                   line2))
          (t
           (concat (truncate-string-to-width line1 index-start nil ?\s)
                   line2
                   (truncate-string-to-width line1 (length line1) (min (length line1) index-end) ?\s)))))
       result-strings))))

(defun company-tip--render-doc-part-above (doc-lines doc-position)
  "Render a part of the doc above the company tooltip.

The 1st arg DOC-LINES is a list containing doc string lines.  The 2nd arg
DOC-POSITION indicates at which side the doc will be rendered."
  (let* ((tooltip-height
          (abs (overlay-get company-pseudo-tooltip-overlay 'company-height)))
         (tooltip-abovep (nth 3 (overlay-get company-pseudo-tooltip-overlay 'company-replacement-args)))
         (n-lines (length doc-lines))

         (ov-end (if tooltip-abovep
                     (company-tip--pos-after-lines
                      (line-beginning-position) (- tooltip-height))
                   (line-beginning-position)))
         (ov-start (company-tip--pos-after-lines ov-end (- n-lines)))
         (buffer-lines (s-lines (buffer-substring ov-start ov-end)))
         (ov (make-overlay ov-start ov-end)))
    (!cons ov company-tip-overlays)
    (--> (company-tip--merge-docstrings buffer-lines doc-lines doc-position)
         (string-join it "\n")
         (if (< (overlay-start ov) (overlay-end ov))
             (overlay-put ov 'display it)
           (overlay-put ov 'before-string it)))
    (overlay-put ov 'window (selected-window))))

(defun company-tip--render-doc-part-current-line (doc-lines doc-position)
  "Render a part of the doc on current line.

The 1st arg DOC-LINES is a list of doc string lines.  The 2nd arg DOC-POSITION
indicates at which side the doc will be rendered."
  (let* ((company-nl
          (nth 2 (overlay-get company-pseudo-tooltip-overlay
                              'company-replacement-args)))
         (current-buffer-line
          (buffer-substring (line-beginning-position) (line-end-position)))
         (tooltip-lines
          (s-lines
           (overlay-get company-pseudo-tooltip-overlay 'company-display)))
         (use-before-string
          (overlay-get company-pseudo-tooltip-overlay 'before-string))
         (ov-start-col
          (save-excursion
            (goto-char (overlay-start company-pseudo-tooltip-overlay))
            (current-column)))
         (tooltip-popped-nl
          (and use-before-string company-nl (pop tooltip-lines))))

    (if (and (eq doc-position 'right) tooltip-popped-nl)

        (--> (->
              (company-tip--merge-docstrings (list current-buffer-line) doc-lines doc-position)
              (string-join "\n")
              (substring ov-start-col))
             (cons it tooltip-lines)
             (string-join it "\n")
             (overlay-put company-pseudo-tooltip-overlay 'company-display it)
             (overlay-put company-pseudo-tooltip-overlay 'before-string it))

      (let* ((ov-start (line-beginning-position))
             (ov-end (line-end-position))
             (ov (make-overlay ov-start ov-end)))
        (!cons ov company-tip-overlays)
        (--> (company-tip--merge-docstrings (list current-buffer-line) doc-lines doc-position)
             (car it)
             (or (put-text-property (current-column) (+ 1 (current-column)) 'cursor (length it) it) it)
             (if (< ov-start ov-end)
                 (overlay-put ov 'display it)
               (overlay-put ov 'before-string it)))
        (overlay-put ov 'window (selected-window))))))

(defun company-tip--render-doc-part-matching-tooltip (doc-lines doc-position)
  "Render a part of the doc matching the company tooltip.

The 1st arg DOC-LINES is a list containing doc string lines.  The 2nd arg
DOC-POSITION indicates at which side the doc will be rendered."
  (let* ((stackwise-p (memq doc-position '(top bottom)))
         (ov company-pseudo-tooltip-overlay)
         (ncandidates (length company-candidates))
         (tooltip-abovep (nth 3 (overlay-get ov 'company-replacement-args)))
         (tooltip-height (abs (overlay-get ov 'company-height)))
         (n-noncandidate-lines (max 0 (- tooltip-height ncandidates)))
         (n-doc-lines (length doc-lines))
         (company-nl (nth 2 (overlay-get ov 'company-replacement-args)))
         (tooltip-lines (s-lines (overlay-get ov 'company-display)))
         (use-before-string (overlay-get ov 'before-string))
         (tooltip-popped-nl
          (and use-before-string company-nl (pop tooltip-lines))))
    (--> (if tooltip-abovep
             (if stackwise-p
                 (append (make-list (max 0 (- n-noncandidate-lines n-doc-lines)) "")
                         doc-lines
                         (make-list ncandidates ""))
               (append (make-list (min n-noncandidate-lines (- tooltip-height n-doc-lines)) "")
                       doc-lines))
           (append (when stackwise-p (make-list ncandidates ""))
                   doc-lines))
         (company-tip--merge-docstrings tooltip-lines it doc-position)
         ;; (company-tip--merge-docstrings tooltip-lines doc-lines doc-position)
         (if tooltip-popped-nl (cons tooltip-popped-nl it) it)
         (string-join it "\n")
         (overlay-put company-pseudo-tooltip-overlay
                      (if use-before-string 'before-string 'display) it))))

(defun company-tip--render-doc-part-below (doc-lines doc-position)
  "Render a part of the doc below the company tooltip.

The 1st arg DOC-LINES is a list containing doc string lines.  The 2nd arg
DOC-POSITION indicates at which side the doc will be rendered."
  (let* ((n-lines (length doc-lines))
         (tooltip-lines
          (s-lines
           (overlay-get company-pseudo-tooltip-overlay 'company-display)))
         (tooltip-height
          (abs (overlay-get company-pseudo-tooltip-overlay 'company-height)))
         (tooltip-abovep
          (nth 3 (overlay-get
                  company-pseudo-tooltip-overlay 'company-replacement-args)))
         (ov-start
          (if tooltip-abovep
              (company-tip--pos-after-lines
               (line-beginning-position) 1)
            (company-tip--pos-after-lines
             (line-beginning-position) (1+ tooltip-height))))
         (ov-end (company-tip--pos-after-lines ov-start (1+ n-lines)))
         (buffer-lines (s-lines (buffer-substring ov-start ov-end)))
         (use-before-string (>= ov-start ov-end))
         (ov (make-overlay ov-start ov-end)))
    (!cons ov company-tip-overlays)
    (--> (company-tip--merge-docstrings buffer-lines doc-lines doc-position)
         (if (and use-before-string
                  (or tooltip-abovep (< (length tooltip-lines) (+ 1 tooltip-height))))
             (cons "" it) it)
         (string-join it "\n")
         (if use-before-string
             (overlay-put ov 'before-string it)
           (overlay-put ov 'display it)))
    (overlay-put ov 'window (selected-window))))

(defun company-tip--get-layout (doc-lines-length)
  "Get the layout for doc parts.  DOC-LINES-LENGTH is the number of lines of doc."
  (let* ((rows-above (cdr (company--col-row (- (point) 1))))
         (rows-below (- (company--window-height) 1 rows-above))
         (tooltip-abovep
          (nth 3
               (overlay-get
                company-pseudo-tooltip-overlay 'company-replacement-args)))
         (tooltip-height
          (min
           (if tooltip-abovep rows-above rows-below)
           (abs (overlay-get company-pseudo-tooltip-overlay 'company-height)))))
    (if tooltip-abovep
        (let* ((nlines-above
                (min (max (- rows-above tooltip-height) 0)
                     (max (- doc-lines-length tooltip-height) 0)))
               (nlines-matching-tooltip
                (min tooltip-height
                     doc-lines-length))
               (nlines-current-line
                (if (> doc-lines-length rows-above) 1 0))
               (nlines-below
                (min rows-below (max (- doc-lines-length rows-above 1) 0)))
               (layout-alist '()))
          (when (> nlines-above 0)
            (!cons `(:above . ,nlines-above) layout-alist))
          (when (> nlines-matching-tooltip 0)
            (!cons `(:matching-tooltip . ,nlines-matching-tooltip) layout-alist))
          (when (> nlines-current-line 0)
            (!cons `(:current-line . ,nlines-current-line) layout-alist))
          (when (> nlines-below 0)
            (!cons `(:below . ,nlines-below) layout-alist))
          (nreverse layout-alist))

      (let* ((nlines-below
              (min (max (- rows-below tooltip-height) 0)
                   (max (- doc-lines-length tooltip-height) 0)))
             (nlines-matching-tooltip
              (min tooltip-height
                   doc-lines-length))
             (nlines-current-line
              (if (> doc-lines-length rows-below) 1 0))
             (nlines-above
              (min rows-above
                   (max (- doc-lines-length rows-below 1) 0)))
             (layout-alist '()))
        (when (> nlines-above 0)
          (!cons `(:above . ,nlines-above) layout-alist))
        (when (> nlines-current-line 0)
          (!cons `(:current-line . ,nlines-current-line) layout-alist))
        (when (> nlines-matching-tooltip 0)
          (!cons `(:matching-tooltip . ,nlines-matching-tooltip) layout-alist))
        (when (> nlines-below 0)
          (!cons `(:below . ,nlines-below) layout-alist))
        (nreverse layout-alist)))))

(defun company-tip--render-sidewise (doc-lines position)
  "Show doc on the right side of company pseudo tooltip.

DOC-LINES is a list of doc string lines.  The 2nd arg POSITION, should be
either 'right, meaning showing the doc on the right side, or 'left, meaning left
side."
  (let* ((layout (company-tip--get-layout (length doc-lines))))
    (dolist (i layout t)
      (let* ((doc-part-lines (cl-subseq doc-lines 0 (cdr i))))
        (setq doc-lines (cl-subseq doc-lines (cdr i)))
        (cond
         ((eq (car i) :above)
          (company-tip--render-doc-part-above doc-part-lines position))
         ((eq (car i) :current-line)
          (company-tip--render-doc-part-current-line doc-part-lines position))
         ((eq (car i) :matching-tooltip)
          (company-tip--render-doc-part-matching-tooltip doc-part-lines position))
         ((eq (car i) :below)
          (company-tip--render-doc-part-below doc-part-lines position)))))))

(defun company-tip--render-stackwise (doc-strings position)
  "Show doc on the top or bottom of company pseudo tooltip.

DOC-STRINGS is a list of doc string lines.  The 2nd arg POSITION, should be
either 'top, meaning showing the doc on the top side, or 'bottom, meaning bottom
side."
  (let* ((ov company-pseudo-tooltip-overlay)
         (ncandidates (length company-candidates))
         (company-nl (nth 2 (overlay-get ov 'company-replacement-args)))
         (tooltip-abovep (nth 3 (overlay-get ov 'company-replacement-args)))
         (tooltip-height (abs (overlay-get ov 'company-height)))
         (tooltip-string (overlay-get ov 'company-display))
         (tooltip-strings
          (cond
           (tooltip-abovep
            (cl-subseq (s-lines tooltip-string) 0 -1))
           (company-nl
            (cl-subseq (s-lines tooltip-string) 1))
           (t (s-lines tooltip-string)))))
    (cond
     ((eq position 'top)
      (if tooltip-abovep
          (let* ((doc-part-matching-tooltip
                  (and (< ncandidates tooltip-height)
                       (append
                        ;; (if (< (length doc-strings) (- tooltip-height ncandidates))
                        ;;     (mapcar
                        ;;      (lambda (l) (substring l (min (length l) (+ 1 (window-hscroll)))))
                        ;;      (cl-subseq tooltip-strings 0 (- (- tooltip-height ncandidates) (length doc-strings)))))
                        (cl-subseq doc-strings (- (min (length doc-strings) (- tooltip-height ncandidates))))
                        ;; (mapcar
                        ;;  (lambda (l) (substring l (min (length l) (+ 1 (window-hscroll)))))
                        ;;  (cl-subseq tooltip-strings (- ncandidates)))
                        )))

                 (doc-part-nlines-above
                  (if doc-part-matching-tooltip
                      (- (length doc-strings);; (+ (length doc-strings) ncandidates)
                         (length doc-part-matching-tooltip))
                    (length doc-strings)))
                 (doc-part-lines-above
                  (and (> doc-part-nlines-above 0)
                       (cl-subseq doc-strings 0 doc-part-nlines-above))))
            (when doc-part-matching-tooltip
              (company-tip--render-doc-part-matching-tooltip doc-part-matching-tooltip position))
            (when doc-part-lines-above
              (company-tip--render-doc-part-above doc-part-lines-above position)))
        (company-tip--render-doc-part-above doc-strings position)))
     ((eq position 'bottom)
      (if tooltip-abovep
          (company-tip--render-doc-part-below doc-strings position)
        (let* ((doc-part-matching-tooltip
                (and (< ncandidates tooltip-height)
                     (append
                      ;; (mapcar
                      ;;  (lambda (l) (substring l (min (length l) (window-hscroll))))
                      ;;  ;; (lambda (l) (substring l (min (length l) (+ 1 (window-hscroll)))))
                      ;;  (cl-subseq tooltip-strings 0 ncandidates))
                      (cl-subseq doc-strings 0 (min (length doc-strings) (- tooltip-height ncandidates))))))

               (doc-part-nlines-below
                (if doc-part-matching-tooltip
                    (- (length doc-strings);; (+ (length doc-strings) ncandidates)
                       (length doc-part-matching-tooltip))
                  (length doc-strings)))
               (doc-part-lines-below
                (and (> doc-part-nlines-below 0)
                     (cl-subseq doc-strings (- doc-part-nlines-below)))))
          (when doc-part-matching-tooltip
            (company-tip--render-doc-part-matching-tooltip doc-part-matching-tooltip position))
          (when doc-part-lines-below
            (company-tip--render-doc-part-below doc-part-lines-below position))))))))

(defun company-tip--show ()
  "Show doc."
  (while-no-input
    (let* ((selected (nth company-selection company-candidates))
           (doc (let ((inhibit-message t))
                  (company-tip--fetch-docstring selected))))
      (when doc
        (let*
            ((ov company-pseudo-tooltip-overlay)
             (tooltip-width (overlay-get ov 'company-width))
             (tooltip-height (abs (overlay-get ov 'company-height)))
             (company-column (overlay-get ov 'company-column))
             (window-width (company--window-width))
             (window-height (company--window-height))
             (horizontal-span (+ window-width (window-hscroll)))
             (tooltip-column (min (+ 1 (- horizontal-span tooltip-width)) company-column))
             (tooltip-abovep (nth 3 (overlay-get ov 'company-replacement-args)))
             (current-row (cdr (company--col-row (- (point) 1))))
             (remaining-cols-right
              (- (+ window-width (window-hscroll)) tooltip-column tooltip-width 2)))
          (when (and ov doc)
            (let (doc-strings-right
                  doc-strings-left
                  doc-strings-top-bottom)
              (or
               ;; Prefer show on right
               (and (> remaining-cols-right company-tip-min-cols-if-show-on-right)
                    (setq doc-strings-right (company-tip--format-string doc remaining-cols-right))
                    (and (<= (length doc-strings-right) window-height)
                         (company-tip--render-sidewise doc-strings-right 'right)))
               ;; If no enough space on the right, show on the side with the most space
               ;; The doc will be truncated if necessary
               (and t
                    (let* ((remaining-cols-left
                            (- tooltip-column (window-hscroll) 5))
                           (remaining-rows-top
                            (- current-row
                               (if tooltip-abovep (min tooltip-height (length company-candidates)) 0)))
                           (remaining-rows-bottom
                            (- window-height current-row
                               (if tooltip-abovep 0 (min tooltip-height (length company-candidates))) 1))
                           (area-right (* remaining-cols-right window-height))
                           (area-left (* remaining-cols-left window-height))
                           (area-top (* remaining-rows-top window-width))
                           (area-bottom (* remaining-rows-bottom window-width)))
                      (cond
                       ((>= area-right (max area-left area-top area-bottom))
                        (or doc-strings-right
                            (setq doc-strings-right (company-tip--format-string doc remaining-cols-right)))
                        (company-tip--render-sidewise
                         (cl-subseq doc-strings-right 0 (min (length doc-strings-right) window-height)) 'right))
                       ((>= area-left (max area-right area-top area-bottom))
                        (setq doc-strings-left (company-tip--format-string doc remaining-cols-left))
                        (company-tip--render-sidewise
                         (cl-subseq doc-strings-left 0 (min (length doc-strings-left) window-height)) 'left))
                       ((>= area-top (max area-right area-left area-bottom))
                        (setq doc-strings-top-bottom (company-tip--format-string doc (- window-width 3)))
                        (company-tip--render-stackwise
                         (cl-subseq doc-strings-top-bottom 0 (min (length doc-strings-top-bottom) remaining-rows-top)) 'top))
                       ((>= area-bottom (max area-right area-left area-top))
                        (setq doc-strings-top-bottom (company-tip--format-string doc (- window-width 3)))
                        (company-tip--render-stackwise
                         (cl-subseq doc-strings-top-bottom 0 (min (length doc-strings-top-bottom) remaining-rows-bottom)) 'bottom)))))))))))))

(defun company-tip--set-timer ()
  "Set timer for tip to pop up."
  (when (or (null company-tip--timer)
            (eq this-command #'company-tip--manual-begin))
    (setq company-tip--timer
          (run-with-idle-timer company-tip-delay nil
                               'company-tip--show))))

(defun company-tip--cancel-timer ()
  "Cancel timer."
  (when (timerp company-tip--timer)
    (cancel-timer company-tip--timer)
    (setq company-tip--timer nil)))

(defun company-tip--enable ()
  "Enable company tip."
  (make-local-variable 'company-frontends)
  (add-to-list 'company-frontends 'company-tip-frontend :append))

(defun company-tip--disable ()
  "Disable company tip."
  (company-tip--cancel-timer)
  (setq-local company-frontends (delq 'company-tip-frontend company-frontends)))

;;;###autoload
(define-minor-mode company-tip-local-mode
  "Provides documentation popups for `company-mode' using `popup-tip'."
  :global nil
  (if company-tip-local-mode
      (company-tip--enable)
    (company-tip--disable)))

;;;###autoload
(define-globalized-minor-mode company-tip-mode
  company-tip-local-mode company-tip-local-mode)

(provide 'company-tip)

;;; company-tip.el ends here
