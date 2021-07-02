;;; -*- lexical-binding: t -*-

(defun +smart-file-name-cached ()
  (if (eq (buffer-name) (car +smart-file-name-cache))
      (cdr +smart-file-name-cache)
    (let ((file-name (+smart-file-name)))
      (setq +smart-file-name-cache
            (cons (buffer-name) file-name))
      file-name)))

(defvar +smart-file-name-cache nil)

(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
This function is slow, so we have to use cache."
  (let ((vc-dir (vc-root-dir))
        (bfn (buffer-file-name (current-buffer))))
    (cond
     ((and bfn vc-dir)
      (file-relative-name bfn vc-dir))
     (bfn bfn)
     (t (buffer-name)))))

(defun +proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,+proxy)
          ("https" . ,+proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)"))))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (proxy-http-disable)
    (proxy-http-enable)))

(defconst tldr-buffer-name "*tldr*")
(defconst tldr-url-template "https://api.github.com/repos/tldr-pages/tldr/contents/pages/%s/%s.md")

;;;###autoload
(defun tldr (cmd &optional op)
  "View tldr page of CMD.
If OP is non-nil and search failed, OP will be used as platform
name and search again. Typically OP is nil or \"common\"."
  (interactive "sCommand: ")
  (let* ((platform (or op
                     (pcase system-type
                       ('gnu "linux")
                       ('gnu/linux "linux")
                       ('darwin "osx")
                       ('ms-dos "windows"))))
         (url (format tldr-url-template platform cmd)))
    (url-retrieve url
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (if (not op)
                            (tldr cmd "common")
                          (user-error "Something went wrong.\n\n%s" (pp-to-string (plist-get status :error))))
                      (search-forward "\n\n")
                      (let* ((req (json-read))
                             (encoding (alist-get 'encoding req))
                             (content (alist-get 'content req)))
                        (cl-assert (string= encoding "base64"))
                        (let ((buf (get-buffer-create tldr-buffer-name))
                              (inhibit-read-only t))
                          (with-current-buffer buf
                            (erase-buffer)
                            (insert (base64-decode-string content))
                            (when (functionp 'markdown-mode)
                              (markdown-mode))
                            (view-mode +1)
                            (pop-to-buffer buf)))))))))

(provide 'init-utils)
