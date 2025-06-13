;;; gptel-commit.el --- Generate commit message with gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Liu Bo

;; Author: Liu Bo <liubolovelife@gmail.com>
;; Keywords: vc

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

;;

;;; Code:

(require 'gptel)

(defgroup gptel-commit nil
  "Generate commit messages with GPTel."
  :group 'vc
  :group 'gptel)

(defcustom gptel-commit-stream t
  "Whether to stream commit message generation.
Set to nil if your backend doesn't support streaming."
  :type 'boolean
  :group 'gptel-commit)

(defcustom gptel-commit-auto-fill t
  "Whether to auto-fill paragraphs after generating commit message."
  :type 'boolean
  :group 'gptel-commit)

(defvar gptel-commit-prompt
  "You are an expert at writing Git commit messages.
Generate **only** the commit message, nothing else.

Critical Rules:
1. For single-file changes with a simple modification:
   - Output ONLY: `* file.el (func): Description.`
   - NO subject line, NO body, JUST the ChangeLog entry

2. For multi-file changes or complex modifications:
   - Subject line (≤50 chars, imperative, capitalize, NO period)
   - Blank line
   - Optional body (ONLY if rationale needed, wrap at 72 chars)
   - ChangeLog entries

3. Special prefixes:
   - If change is trivial (typo/comment/docs), prefix with `; `
   - Example: `; * file.el: Fix typo.`

Decision tree:
- Single file + simple change → Format 1
- Multiple files OR complex change → Format 2
- Trivial change → Add `; ` prefix

Examples:

Single file, simple change:

* configure.ac: Detect Android API 36.

Multiple files:

New user option: vc-allow-async-diff

Centralize the control for whether 'vc-diff' is asynchronous
across the backends, while also allowing 'vc-BACKEND-diff' to be
called asynchronously when needed.  Previously in bug#21969.

* lisp/vc/vc.el (vc-allow-async-diff): New user option.

* lisp/vc/vc.el (vc-version-diff, vc-diff, vc-diff-mergebase)
(vc-root-diff): Obey it.

Single file, trivial change:

; * src/w32dwrite.c: Minor coding style adjustments."
  "A prompt adapted from Emacs.")

(defvar gptel-commit-after-insert-hook nil
  "Hook run when gptel insert commit message.")

(defvar gptel-commit-backend gptel-backend
  "The backend used specifically for generating commit messages with `gptel-commit`.
This can be set to a lightweight or free model (e.g., via OpenRouter),
so it won't interfere with your default `gptel` usage for general chat.")

(defvar gptel-commit-diff-excludes
  '("pnpm-lock.yaml"
    "*.lock"
    "ent/**/*.go")
  "List of file globs to exclude from commit diff analysis.")

(defvar gptel-commit--current-buffer nil
  "Buffer where commit message is being generated.")

(defvar gptel-commit-rationale-buffer "*GPTel Commit Rationale*"
  "Buffer name for entering rationale for commit message generation.")

(defun gptel--wildcard-to-regexp (glob)
  "Convert shell glob GLOB to a regular expression."
  (let ((glob (replace-regexp-in-string "\\.\\*" ".*" glob)))
    (wildcard-to-regexp glob)))

(defun gptel--excluded-file-p (filename)
  "Check if FILENAME matches any pattern in `gptel-commit-diff-excludes`."
  (cl-some (lambda (pat)
             (string-match-p (gptel--wildcard-to-regexp pat) filename))
           gptel-commit-diff-excludes))

(defun gptel-commit--filtered-diff ()
  "Return a filtered diff string of staged changes, excluding patterns."
  (let* ((files (split-string
                 (shell-command-to-string "git diff --name-only --cached")
                 "\n" t))
         (included-files (cl-remove-if #'gptel--excluded-file-p files))
         (diffs '()))
    (dolist (file included-files)
      (let ((diff (shell-command-to-string (format "git diff --cached -- %s" file))))
        (when (not (string-empty-p diff))
          (push (format "===== %s =====\n%s" file diff) diffs))))
    (string-join (nreverse diffs) "\n\n")))

(defun gptel-commit--find-commit-buffer ()
  "Find the appropriate buffer for commit message."
  (or (get-buffer "COMMIT_EDITMSG")
      (and (derived-mode-p 'text-mode 'git-commit-mode) (current-buffer))
      (user-error "No commit message buffer found")))

(defun gptel-commit--stream-callback (response info)
  "Stream callback for gptel responses in commit buffer."
  (when-let* ((buffer gptel-commit--current-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (insert response))))))

(defun gptel-commit--setup-request-args ()
  "Setup request arguments based on streaming preference."
  (if gptel-commit-stream
      (list :callback #'gptel-commit--stream-callback
            :stream t)
    nil))

(defun gptel-commit--post-process ()
  "Post-process generated commit message."
  (when (and gptel-commit-auto-fill gptel-commit--current-buffer)
    (with-current-buffer gptel-commit--current-buffer
      (save-excursion
        (goto-char (point-min))
        ;; Skip subject line
        (forward-line 1)
        (while (not (eobp))
          (unless (looking-at "^$")
            (fill-paragraph))
          (forward-line 1))))))

(defun gptel-commit--generate-message (rationale)
  "Generate commit message with optional RATIONALE."
  (let* ((changes (gptel-commit--filtered-diff))
         (prompt (if (and rationale (not (string-empty-p rationale)))
                     (format "Context: %s\n\nChanges:\n%s" rationale changes)
                   changes))
         (gptel-backend gptel-commit-backend)
         (buffer (gptel-commit--find-commit-buffer)))
    (if (string-empty-p changes)
        (message "No staged changes to commit.")
      (setq gptel-commit--current-buffer buffer)
      (with-current-buffer buffer
        ;; Clear buffer if streaming
        (when gptel-commit-stream
          (erase-buffer))
        ;; Make request
        (apply #'gptel-request prompt
               :system gptel-commit-prompt
               (gptel-commit--setup-request-args)))
      ;; Run hooks after a delay for non-streaming mode
      (if gptel-commit-stream
          (run-hooks 'gptel-commit-after-insert-hook)
        (run-with-timer 0.5 nil
                        (lambda ()
                          (run-hooks 'gptel-commit-after-insert-hook)))))))

;; Rationale mode and functions
(define-derived-mode gptel-commit-rationale-mode text-mode "GPTel-Commit-Rationale"
  "Mode for entering commit rationale before GPTel generates commit message."
  (local-set-key (kbd "C-c C-c") #'gptel-commit--submit-rationale)
  (local-set-key (kbd "C-c C-k") #'gptel-commit--cancel-rationale))

(defun gptel-commit--setup-rationale-buffer ()
  "Setup the rationale buffer with proper guidance."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert ";;; WHY are you making these changes? (optional)\n")
    (insert ";;; Press C-c C-c to generate commit message, C-c C-k to cancel\n")
    (insert ";;; Leave empty to generate without rationale\n")
    (insert ";;; ────────────────────────────────────────────────────────\n\n")
    (add-text-properties (point-min) (point)
                         '(face font-lock-comment-face read-only t))
    (goto-char (point-max))))

(defun gptel-commit--submit-rationale ()
  "Submit the rationale buffer content and proceed with GPTel commit generation."
  (interactive)
  (let ((rationale (string-trim
                    (buffer-substring-no-properties
                     (save-excursion
                       (goto-char (point-min))
                       (forward-line 4)
                       (point))
                     (point-max)))))
    (kill-buffer gptel-commit-rationale-buffer)
    (gptel-commit--generate-message rationale)))

(defun gptel-commit--cancel-rationale ()
  "Cancel rationale input and abort GPTel commit generation."
  (interactive)
  (kill-buffer gptel-commit-rationale-buffer)
  (message "GPTel commit generation canceled."))

;; Public functions
;;;###autoload
(defun gptel-commit ()
  "Generate commit message with gptel."
  (interactive)
  (gptel-commit--generate-message nil))

;;;###autoload
(defun gptel-rationale-commit ()
  "Prompt user for rationale and generate commit message with GPTel."
  (interactive)
  ;; Save the current buffer context if in a commit buffer
  (when (or (get-buffer "COMMIT_EDITMSG")
            (derived-mode-p 'text-mode 'git-commit-mode))
    (setq gptel-commit--current-buffer (current-buffer)))

  (let ((buffer (get-buffer-create gptel-commit-rationale-buffer)))
    (with-current-buffer buffer
      (gptel-commit-rationale-mode)
      (gptel-commit--setup-rationale-buffer))
    (pop-to-buffer buffer)))

;;;###autoload
(defun gptel-commit-magit ()
  "Generate commit message for use with Magit."
  (interactive)
  (if (derived-mode-p 'text-mode 'git-commit-mode)
      (gptel-commit)
    (user-error "Not in a commit message buffer")))

;;;###autoload
(defun gptel-rationale-commit-magit ()
  "Generate commit message with rationale for use with Magit."
  (interactive)
  (if (derived-mode-p 'text-mode 'git-commit-mode)
      (gptel-rationale-commit)
    (user-error "Not in a commit message buffer")))

;;;###autoload
(defun gptel-commit-fill-paragraph ()
  "Fill paragraph in commit message."
  (interactive)
  (when-let* ((buffer (get-buffer "COMMIT_EDITMSG")))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (forward-line 2)
        (fill-paragraph)))))

;; Add post-processing to hook
(add-hook 'gptel-commit-after-insert-hook #'gptel-commit--post-process)

;; Key bindings suggestions (users can add to their config)
(with-eval-after-load 'magit
  (define-key git-commit-mode-map (kbd "C-c g") #'gptel-commit-magit)
  (define-key git-commit-mode-map (kbd "C-c G") #'gptel-rationale-commit-magit))

(provide 'gptel-commit)

;;; gptel-commit.el ends here
