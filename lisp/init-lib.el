;;; init-lib.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(install-package 'popon)
(install-package 'fullframe)
(install-package 'hide-mode-line)

;; run `nerd-icons-install-fonts'
;; ttf-nerd-fonts-symbols-1000-em-mono
(install-package 'nerd-icons)

(install-package 'visual-fill-column)

(install-package 'valign)
(setq valign-fancy-bar t)
(install-package 'ftable)
(install-package 'mixed-pitch)

;;; Useful funcs

;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(defun memoize-remote (key cache orig-fn &rest args)
  "Memoize a value if the key is a remote path."
  (if (and key
           (file-remote-p key))
      (if-let* ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))

(defun my/url-get-title (url &optional descr)
  "Takes a URL and returns the value of the <title> HTML tag.
   This function uses curl if available, and falls back to url-retrieve if not.
   It also handles UTF-8 encoded titles correctly."
  (when (or (string-prefix-p "http" url)
            (string-prefix-p "https" url))
    (let ((curl-available (executable-find "curl")))
      (with-temp-buffer
        (if curl-available
            (call-process "curl" nil t nil "-s" url)
          (let ((url-buf (url-retrieve-synchronously url)))
            (when url-buf
              (insert-buffer-substring url-buf)
              (kill-buffer url-buf))))
        (goto-char (point-min))
        (if (search-forward-regexp "<title>\\([^\n]+?\\)</title>" nil t)
            (decode-coding-string (match-string 1) 'utf-8)
          "No title found")))))

(defun retrieve-authinfo-key (host user)
  "从 .authinfo 中检索指定 HOST 和 USER 的密钥。"
  (interactive "sEnter host: \nsEnter user: ") ; 交互式输入 host 和 user
  ;; 使用 auth-source-search 来搜索匹配的条目
  (let ((credentials (auth-source-search :host host
                                         :user user
                                         :require '(:secret) ; 确保结果中包含密钥
                                         :max 1))) ; 最多返回一个结果
    (if credentials
        ;; 如果找到了凭据，使用 auth-source-secret 函数解析并返回密钥
        (let ((secret (funcall (plist-get (car credentials) :secret))))
          secret)
      ;; 如果没有找到凭据，显示消息
      (message "No credentials found for %s@%s." user host))))

(defun move-region-to-trash (start end)
  "Move the selected region to trash.el."
  (interactive "r")
  (let ((region-content (buffer-substring start end))
        (trash-file (expand-file-name "trash.el" user-emacs-directory)))
    ;; Ensure the file exists
    (unless (file-exists-p trash-file)
      (with-temp-buffer (write-file trash-file)))
    ;; Append the content to the trash file
    (with-temp-file trash-file
      (insert-file-contents trash-file)
      (goto-char (point-max))
      (insert "\n" region-content "\n"))
    ;; Optionally, delete the region from the original buffer
    (delete-region start end)))

;; https://www.emacswiki.org/emacs/BuildTags
;; Or generate manually, an expample for go file:
;; find . -type f -iname "*.go" | etags -
(defun create-etags (dir-name file-extension)
  "Create tags file in DIR-NAME for files matching FILE-EXTENSION."
  (interactive
   (list (read-directory-name "Directory: ")
         (read-regexp "Iname regexp (e.g., *.go): ")))
  (eshell-command
   (format "find %s -type f -iname \"%s\" | etags -" dir-name file-extension)))

(defun get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun format-second-timestamp (begin end)
  "Convert the selected region (a timestamp in seconds) to a formatted time string."
  (interactive "r")
  (let* ((timestamp-str (buffer-substring-no-properties begin end))
         (timestamp (string-to-number timestamp-str))
         (formatted-time (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time timestamp))))
    (message "%s" formatted-time)))

;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(keymap-substitute global-map #'move-beginning-of-line #'smarter-move-beginning-of-line)

(defun my/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha-background) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha-background newalpha))))))
(global-set-key (kbd "M-C-8") (lambda () (interactive) (my/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (my/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha-background . 100)))))

(defun my/delete-to-the-begining ()
  (interactive)
  (delete-region (point-min) (point)))

(defun my/delete-to-the-end ()
  (interactive)
  (delete-region (point) (point-max)))

(defun my/delete-whole-buffer ()
  (interactive)
  (delete-region (point-min) (point-max)))

(defun my/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-current-buffer)))

;;; init-lib.el ends here
