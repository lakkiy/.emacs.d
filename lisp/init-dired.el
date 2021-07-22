;;; -*- lexical-binding: t -*-

(eat-package dired-hacks
  :straight (dired-hacks :type git
                         :host github
                         :repo "Fuco1/dired-hacks")
  :init
  (eat-package dired-filter
    :hook (dired-mode-hook . dired-filter-group-mode)
    :init
    (setq dired-filter-revert 'never
          dired-filter-group-saved-groups
          '(("default"
             ("Git"
              (directory . ".git")
              (file . ".gitignore"))
             ("Directory"
              (directory))
             ("PDF"
              (extension . "pdf"))
             ("LaTeX"
              (extension "tex" "bib"))
             ("Source"
              (extension "c" "cpp" "hs" "rb" "py" "r" "cs" "el" "lisp" "html" "js" "css"))
             ("Doc"
              (extension "md" "rst" "txt"))
             ("Org"
              (extension . "org"))
             ("Archives"
              (extension "zip" "rar" "gz" "bz2" "tar"))
             ("Images"
              (extension "jpg" "JPG" "webp" "png" "PNG" "jpeg" "JPEG" "bmp" "BMP" "TIFF" "tiff" "gif" "GIF")))))
    :config
    (define-key dired-filter-map (kbd "p") 'dired-filter-pop-all)
    (define-key dired-filter-map (kbd "/") 'dired-filter-mark-map))
  (eat-package dired-narrow
    :config
    (define-key dired-narrow-map (kbd "<down>") 'dired-narrow-next-file)
    (define-key dired-narrow-map (kbd "<up>") 'dired-narrow-previous-file)
    (define-key dired-narrow-map (kbd "<right>") 'dired-narrow-enter-directory))
  (eat-package dired-collapse
    :hook (dired-mode-hook . dired-collapse-mode))
  (eat-package dired-rainbow
    :commands dired
    :config
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#705438" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))
  (eat-package dired-subtree
    :after dired
    :config
    (define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
    (define-key dired-mode-map (kbd ";") 'dired-subtree-remove)))

(eat-package dired
  :config
  (define-key dired-mode-map (kbd "C-c C-h") 'dired-hydra/body)
  :init
  (pretty-hydra-define dired-hydra (:title "Dired" :quit-key "q")
    ("TODO One"
     (("(" dired-hide-details-mode)
      (")" dired-omit-mode)
      ("+" dired-create-directory)
      ("=" diredp-ediff)         ;; smart diff
      ("?" dired-summary)
      ("$" diredp-hide-subdir-nomove)
      ("A" dired-do-find-regexp)
      ("C" dired-do-copy)        ;; Copy all marked files
      ("D" dired-do-delete)
      ("E" dired-mark-extension)
      ("e" dired-ediff-files))
     "TODO Two"
     (("F" dired-do-find-marked-files)
      ("G" dired-do-chgrp)
      ("g" revert-buffer)        ;; read all directories again (refresh)
      ("i" dired-maybe-insert-subdir)
      ("l" dired-do-redisplay)   ;; relist the marked or singel directory
      ("M" dired-do-chmod)
      ("m" dired-mark)
      ("O" dired-display-file)
      ("o" dired-find-file-other-window)
      ("Q" dired-do-find-regexp-and-replace)
      ("R" dired-do-rename))
     "TODO Three"
     (("r" dired-do-rsynch)
      ("S" dired-do-symlink)
      ("s" dired-sort-toggle-or-edit)
      ("t" dired-toggle-marks)
      ("U" dired-unmark-all-marks)
      ("u" dired-unmark)
      ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
      ("w" dired-kill-subdir)
      ("Y" dired-do-relsymlink)
      ("z" diredp-compress-this-file)
      ("Z" dired-do-compress)))))

(provide 'init-dired)
