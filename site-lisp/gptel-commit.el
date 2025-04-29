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

(defconst gptel-commit-prompt
  "Please write a commit message for the following `git diff --cached` output, following **GNU Emacs commit conventions**.

**General requirements:**

- Message must contain only printable UTF-8 (ASCII if possible) characters.
- Write in American English, use present tense.
- Do NOT add lines like 'Signed-off-by' or any other metadata.

**Format:**

1. **Summary line:**
   - One concise, unindented line describing what the change does (not what it did).
   - Use present tense.
   - Start with a capital letter, and do not end with a period.
   - Preferably no longer than 50 characters; must not exceed 78 characters.
   - In most cases, use a brief English description, not a ChangeLog-style entry.
   - Only if the change affects a single file and the result remains concise, you *may* write the summary line as a ChangeLog entry (starting with `* file/name (function): ...`). Avoid this if it would make the summary line too long or harder to read.
   - If the summary line starts with `; ` (semicolon and a space), this commit will be ignored when automatically generating the ChangeLog (this is mainly for trivial or non-code changes, such as documentation edits).

2. **Blank line**

3. **ChangeLog entries:**
   - Each entry starts with an asterisk, file name, and in parentheses a comma-separated list of each affected function/variable; then a colon and a complete sentence describing the change.
     Example:
     * lisp/foo.el (func1, func2): Describe the change.
   - Sentences start with a capital and end with a period.
   - If a change affects multiple functions or variables **in the same file in a similar way**, combine them into one entry (group them all in the parentheses) and use a single description.
   - If a change affects functions/variables in different files in a similar way, you may combine entries for those files for brevity.
   - Do not write a separate entry for each minor similar change if they can be grouped.
   - If related to a bug, add '(Bug#12345)' at the appropriate place.
   - **Hard line length limit: 78 characters** for any line (never exceed, except single long words, up to 140 chars).
   - **Soft line length suggestion: 63 characters**; if a line is longer, break at a space to continue on the next line (with no extra indentation).
   - Do not include files like NEWS or MAINTAINERS unless absolutely necessary.

**Examples(split with ---):**

Fix error handling in foo.el

* lisp/foo.el (my-func, other-func): Check for nil arg.
* src/bar.c (bar_func): Fix memory leak.
  Improve docstring.  (Bug#12345)

---

; Fix typos in documentation

* doc/emacs/commands.texi: Fix minor spelling errors.

---

; Merge from savannah/emacs-30

---

* etc/NEWS: Presentational fixes and improvements.

---

; * doc/emacs/maintaining.texi (VC-Aware Project Backend): Copyedit.

---

Now, for the following `git diff --cached`, generate a **single formatted commit message** only: do not output any explanation, extra commentary, or repeated boilerplate; follow the above guidance and examples.")

(defvar gptel-commit-diff-excludes
  '("pnpm-lock.yaml"
    "pnpm.lock"
    "ent/**/*.go")
  "List of file globs to exclude from commit diff analysis.")

(defun gptel--excluded-file-p (filename)
  "Check if FILENAME matches any pattern in `gptel-commit-diff-excludes`."
  (cl-some (lambda (pat) (string-match-p (wildcard-to-regexp pat) filename))
           gptel-commit-diff-excludes))

(defun gptel-commit--filtered-diff ()
  "Return staged diff with excluded files filtered out."
  (let* ((all-lines (magit-git-lines "diff" "--cached"))
         (result '())
         (skip nil))
    (dolist (line all-lines (nreverse result))
      (if (string-match "^diff --git a/\\(.+\\) b/" line)
          (setq skip (gptel--excluded-file-p (match-string 1 line))))
      (unless skip
        (push line result)))))

;;;###autoload
(defun gptel-commit ()
  "Generate commit message with gptel, ignoring unwanted files."
  (interactive)
  (let ((changes (string-join (gptel-commit--filtered-diff) "\n")))
    (gptel-request changes :system gptel-commit-prompt)))

(provide 'gptel-commit)

;;; gptel-commit.el ends here
