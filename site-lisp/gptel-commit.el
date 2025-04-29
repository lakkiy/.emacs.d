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

**Requirements:**

- The message should contain only printable UTF-8 (ASCII if possible) characters.
- No 'Signed-off-by' or similar metadata lines.
- Write in American English.

**Format:**

1. **Summary line:**
   - One concise, unindented sentence describing what the change does (not what it did), in present tense.
   - The first letter is capitalized.
   - Do **not** end with a period.
   - **Preferably 50 characters or less** (soft limit), but may exceed if necessary.
   - If the summary line starts with '; ', the message will be skipped by ChangeLog tools (normally not needed for regular code commits!).

2. **Blank line**

3. **ChangeLog entries:**
   - List each changed file and function/variable, one per line (multiple related entries can be grouped).
   - Format:
     * file/name.ext (function1, function2): Full sentence describing the change.
   - Use present tense.
   - Sentences start with a capital and end with a period.
   - If relevant to a bug, include '(Bug#NNNNN)' at the proper place.
   - **Hard line length limit: 78 characters per line** (absolutely must not exceed, except for a single very long word, which is rare and up to 140 chars).
   - **Soft line length recommendation: 63 characters per line**.
   - If an entry exceeds 63 characters, break at a space (without indenting the continuation).
   - Lines after the first in an entry should not be indented.
   - Do not include files like NEWS or MAINTAINERS unless they are critical.

**Example 1:**

Improve error handling in foo.el

* lisp/foo.el (my-func): Check for nil arg.
* src/bar.c (bar_func): Fix memory leak.
  Improve docstring.  (Bug#12345)

**Example 2:**

Port Grep argument autodetection to Android

* lisp/progmodes/grep.el (grep-hello-file): On Android, copy
sample text to a real directory.

Now, for the following `git diff --cached`, generate only a properly formatted commit message. Do not add any explanation or commentary."
  )

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
