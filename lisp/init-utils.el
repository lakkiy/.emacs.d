;;; -*- lexical-binding: t -*-

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

;; 不知道为什么现在在 tramp 上执行 vc-region-history
;; 乱码或者颜色不对，例如 ^[33m，目前先用这个函数救急
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))


;; Set PATH and `exec-path'
;; https://emacs-china.org/t/emacs-mac-port-profile/2895/29?u=rua
;; NOTE: When PATH is changed, run the following command
;; $ sh -c 'printf "%s" "$PATH"' > ~/.path
(defun my/getenv-path()
  (interactive)
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))

(when (file-exists-p "~/.path")
  (add-hook 'after-init-hook #'my/getenv-path))


;; Delete things(don’t send to kill ring
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


;; window
(keymap-global-set "M-o" 'other-window)

;; Skip window by setting no-other-window window parameter in
;; display-buffer-alist for specific buffer(like dired-sidebar).

;; When splitting window, show (other-buffer) in the new window
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window with other-buffer unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (unless arg
        (set-window-buffer target-window (other-buffer))
        (select-window target-window)))))

(keymap-global-set "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically))
(keymap-global-set "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(keymap-global-set "C-x 1" 'sanityinc/toggle-delete-other-windows)

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(keymap-global-set "C-x |" 'split-window-horizontally-instead)
(keymap-global-set "C-x _" 'split-window-vertically-instead)


;; tab-bar
;;
;; Built-in window layout manager
;; NOTE do not bind =tab-bar-switch-to-prev-tab= and
;; =tab-bar-switch-to-next-tab= to =M-[= or =M-]=, it will make emacs have some
;; bug to auto insert characters after you type everytime.
;;
;; See =tab-prefix-map= to custom key bindings for tab-bar, default is =C-x t=.
(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item
              (format " %s  "
                      (nerd-icons-sucicon "nf-custom-emacs"
                                          :face '(:inherit nerd-icons-purple)))
              tab-bar-menu-bar :help "Menu Bar")))

(defun my/bar-image ()
  (when (and (display-graphic-p) (image-type-available-p 'pbm))
    (propertize
     " " 'display
     (ignore-errors
       (create-image
        ;; 20 for `dirvish-header-line-height'
        (concat (format "P1\n%i %i\n" 2 30) (make-string (* 2 30) ?1) "\n")
        'pbm t :foreground (face-background 'highlight) :ascent 'center)))))

(setq tab-bar-new-tab-choice 'ibuffer
      tab-bar-close-last-tab-choice 'tab-bar-mode-disable
      tab-bar-tab-hints nil
      tab-bar-close-button-show nil
      tab-bar-separator ""
      tab-bar-format '(tab-bar-format-menu-bar
                       tab-bar-format-tabs)
      ;; NOTE 如果要用到很多 tab 导致 tab 换行的话就把这个设置为 t
      tab-bar-auto-width nil
      tab-bar-tab-name-format-function
      (lambda (tab i) "Center, taller, better, stronger xD."
        (let* ((current-tab-p (eq (car tab) 'current-tab))
               (bar (when current-tab-p (my/bar-image)))
               (name (string-trim (alist-get 'name tab)))
               (space-to-add (max 0 (- tab-bar-tab-name-truncated-max (length name))))
               (left-padding (/ space-to-add 2))
               (right-padding (- space-to-add left-padding)))
          (concat
           bar
           (propertize (concat ;; (propertize " " 'display '(raise 0.3))
                        (make-string left-padding ?\s)
                        name
                        (make-string right-padding ?\s)
                        ;; (propertize " " 'display '(raise -0.3))
                        )
                       'face (funcall tab-bar-tab-face-function tab)))))
      tab-bar-tab-name-function
      (lambda nil "Use project as tab name."
        (let ((dir (expand-file-name
                    (or (if (and (fboundp 'project-root) (project-current))
                            (project-root (project-current)))
                        default-directory))))
          (or
           (and dir
                (let ((name (substring dir (1+ (string-match "/[^/]+/$" dir)) -1)))
                  (truncate-string-to-width name tab-bar-tab-name-truncated-max nil ? )))
           (buffer-name)))))

(with-eval-after-load 'tab-bar
  (tab-bar-history-mode 1))

;;; init-utils.el ends here
