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
  "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes, following **GNU Emacs commit conventions**.

If you can accurately express the change in just the subject line, don't include anything in the message body. Only use the body when it is providing *useful* information.

Don't repeat information from the subject line in the message body.

Only return the commit message in your response. Do not include any additional meta-commentary about the task. Do not include the raw diff output in the commit message.

**General requirements:**

- Message must contain only printable UTF-8 characters (ASCII if possible).
- Write in American English, use present tense.
- Do NOT add lines like 'Signed-off-by' or any other metadata.

**Format:**

1. **Summary line:**
   - One concise, unindented line describing what the change does (not what it did).
   - Start with a capital letter and do not end with a period.
   - Preferably no longer than 50 characters; must not exceed 78 characters.
   - In most cases, use a brief English description, not a ChangeLog-style entry.
   - Only if the change affects a single file and the result remains concise, you *may* write the summary line as a ChangeLog entry (starting with `* file/name (function): ...`). Avoid this if it would make the summary line too long or harder to read.
   - If the summary line starts with ;  (semicolon and a space), it will be *excluded* from the generated ChangeLog. Use this *only for trivial or non-functional changes*, such as typos, comment tweaks, or edits to files like etc/NEWS.

2. **Blank line**

3. **ChangeLog entries(optional, omit it entirely if not useful):**
   - Each entry starts with an asterisk, file name, and in parentheses a comma-separated list of each affected function/variable; then a colon and a complete sentence describing the change.
     Example:
     * lisp/foo.el (func1, func2): Describe the change.
   - Sentences start with a capital and end with a period.
   - If a change affects multiple functions or variables **in the same file in a similar way**, combine them into one entry (group them all in the parentheses) and use a single description.
   - If a change affects functions/variables in different files in a similar way, you may combine entries for those files for brevity.
   - Do not write a separate entry for each minor similar change if they can be grouped.
   - **Hard line length limit: 78 characters** for any line (never exceed, except single long words, up to 140 chars).
   - **Soft line length suggestion: 63 characters**; if a line is longer, break at a space to continue on the next line (with no extra indentation).
   - Do not include files like NEWS or MAINTAINERS unless absolutely necessary."
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
      (gptel-request changes :system gptel-commit-prompt)
      (run-hooks 'gptel-commit-after-insert-hook))))

(provide 'gptel-commit)

;;; gptel-commit.el ends here
