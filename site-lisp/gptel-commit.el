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

(defvar gptel-commit-prompt
  "You are an expert at writing Git commit messages for projects that maintain a ChangeLog.
Generate **only** the commit message, nothing else.

Format:
1. Subject line
   - One concise, unindented line describing what the change does (not what it did).
   - ≤50 characters if possible (hard limit 78).
   - Imperative mood, start with capital letter, **do not end with a period**.
2. Blank line
3. Optional body
   - **Only include** if important context or rationale (why) cannot fit in the subject.
   - Body should explain **why** the change was made, not repeat the subject.
   - Wrap at 72 characters (hard limit 78), written in one or more paragraphs.
4. ChangeLog entries
   - One `* file/path (func1, func2): Description.` per file (or grouped).
   - Sentence case, end with period, wrap at 72 characters (hard limit 78).
   - Omit entries for trivial docs/comments/NEWS updates.

Rules:
- Subject must not end with a period or punctuation.
- Do not include a body unless necessary to explain rationale.
- Don’t repeat the subject in the body.
- Don’t include raw diffs or meta-commentary.
- Use ChangeLog-style entries only if multiple files/functions are involved;
  for a single-file concise change you may fold subject into one entry:
    `* file.el (func): Short description.`
- If the subject begins with `; `(semicolon and space), omit it from the ChangeLog—reserve this only for trivial or non-functional edits, such as typos, comment tweaks, or NEWS updates.

Example:
```text
Flymake: promptly delete eol overlay if source overlay changed

In the vast majority of cases, changing the source overlay
invalidates the content of the end-of-line overlay, so best to
delete it asap.

* lisp/progmodes/flymake.el (flymake--delete-overlay): Use
'flymake--eol-ov'
(flymake--highlight-line): Use some overlay modification hooks.
```
"
  "A prompt adapted from Zed (https://github.com/zed-industries/zed/blob/main/crates/git_ui/src/commit_message_prompt.txt)
  and Emacs(https://github.com/emacs-mirror/emacs/blob/fa05cfd4455f2883d16992e5f1323a8945956987/CONTRIBUTE#L194).")

(defvar gptel-commit-after-insert-hook nil
  "Hook run when gptel insert commit message.")

;; This is a free model and enough to generate commit message for now.
;;
;; (gptel-make-openai "OpenRouter"
;;   :host "openrouter.ai"
;;   :endpoint "/api/v1/chat/completions"
;;   :stream t
;;   :key "KEY"
;;   :models '(qwen/qwen3-30b-a3b:free))
(defvar gptel-commit-backend gptel-backend
  "The backend used specifically for generating commit messages with `gptel-commit`.
This can be set to a lightweight or free model (e.g., via OpenRouter),
so it won't interfere with your default `gptel` usage for general chat.")

(defvar gptel-commit-diff-excludes
  '("pnpm-lock.yaml"
    "*.lock"
    "ent/**/*.go")
  "List of file globs to exclude from commit diff analysis.")

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

(defun gptel-commit-fill-paragraph ()
  (interactive)
  (with-current-buffer "COMMIT_EDITMSG"
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (fill-paragraph))))

;;;###autoload
(defun gptel-commit ()
  "Generate commit message with gptel, ignoring unwanted files."
  (interactive)
  (let ((changes (gptel-commit--filtered-diff))
        (gptel-backend gptel-commit-backend))
    (if (string-empty-p changes)
        (message "No staged changes to commit.")
      (with-current-buffer "COMMIT_EDITMSG"
        (gptel-request changes :system gptel-commit-prompt))
      (run-hooks 'gptel-commit-after-insert-hook))))

(defvar gptel-commit-rationale-buffer "*GPTel Commit Rationale*"
  "Buffer name for entering rationale for commit message generation.")

(defun gptel-commit--submit-rationale ()
  "Submit the rationale buffer content and proceed with GPTel commit generation."
  (interactive)
  (let ((rationale (string-trim (buffer-string))))
    (kill-buffer gptel-commit-rationale-buffer)
    (gptel-commit--generate-message rationale)))

(defun gptel-commit--cancel-rationale ()
  "Cancel rationale input and abort GPTel commit generation."
  (interactive)
  (kill-buffer gptel-commit-rationale-buffer)
  (message "GPTel commit generation canceled."))

(defun gptel-commit--generate-message (rationale)
  "Generate commit message using GPTel with optional RATIONALE."
  (let* ((changes (gptel-commit--filtered-diff))
         (prompt (if (string-empty-p rationale)
                     changes
                   (format "IMPORTANT: The following line explains **WHY** these changes were made. Prioritize this rationale when generating the commit message:\n\n%s\n\nHere are the actual code change diffs:\n%s"
                           rationale changes)))
         (gptel-backend gptel-commit-backend))
    (if (string-empty-p changes)
        (message "No staged changes to commit.")
      (with-current-buffer "COMMIT_EDITMSG"
        (gptel-request prompt :system gptel-commit-prompt))
      (run-hooks 'gptel-commit-after-insert-hook))))

(define-derived-mode gptel-commit-rationale-mode text-mode "GPTel-Commit-Rationale"
  "Mode for entering commit rationale before GPTel generates commit message."
  (local-set-key (kbd "C-c C-c") #'gptel-commit--submit-rationale)
  (local-set-key (kbd "C-c C-k") #'gptel-commit--cancel-rationale)
  (message "Enter rationale for commit. Press C-c C-c when done, or C-c C-k to cancel."))

;;;###autoload
(defun gptel-rationale-commit ()
  "Prompt user for rationale and generate commit message with GPTel."
  (interactive)
  (with-current-buffer (get-buffer-create gptel-commit-rationale-buffer)
    (erase-buffer)
    (gptel-commit-rationale-mode)
    (pop-to-buffer (current-buffer))))

(provide 'gptel-commit)

;;; gptel-commit.el ends here
