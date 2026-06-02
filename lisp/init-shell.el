;;; init-shell.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; term + shell

(install-package 'ghostel)

;; 原生模块放到包目录外面:ghostel 从 MELPA 升级时整个包目录会被替换,
;; 放在包目录里的 ghostel-module.dylib 会跟着被删,每次升级都要重新下载。
;; 首次用到时按提示 M-x ghostel-download-module 下载一次即可。
(setq ghostel-module-directory (expand-file-name "ghostel/" user-emacs-directory))


;;; ghostel × meow
;;
;; ghostel(claude-code-ide 的终端后端)自带一套输入模态(semi-char/char/emacs/
;; copy/line)。在 meow 下加一层桥接,让 meow 状态当唯一开关,只需记两个状态:
;;
;;   INSERT = ghostel-semi-char-mode   直通打字(键直达 shell / Claude)
;;   NORMAL = ghostel-emacs-mode       只读 + 保持 live,导航/搜索/复制/翻历史
;;
;; 默认进 INSERT —— 开终端即可打字;按 ESC 进 NORMAL 读/翻历史,按 i 回来继续打。
;; 在 NORMAL 里 C-p/C-n、meow 的 n/p、v/V 翻历史;终端狂刷时深度上翻用鼠标滚轮。
;; 别手动碰 ghostel 自带的模态命令(C-c C-e / C-c M-d 之类),让 meow 当唯一开关。

;; 初始状态显式钉成 insert:否则 meow 会靠"探测 a-z 是否绑到 self-insert"来猜,
;; 而 ghostel 只读态把字母 remap 成退出命令(名字不含 self-insert),会被误判成
;; motion —— 那样 i/ESC 都进不去。
(with-eval-after-load 'meow
  (add-to-list 'meow-mode-state-list '(ghostel-mode . insert)))

;; NORMAL 用 emacs 模式:只读但保持 live,可用 Emacs/meow 命令在已物化的 scrollback
;; 上导航、搜索、复制,终端在后台继续刷新。注意:终端正在大量输出时,重绘会把
;; window-start 拽回 viewport,键盘深度上翻会被打架 —— 那种时刻用鼠标滚轮(走
;; ghostel 自己的 scroll-intercept);静止时键盘 C-p/C-n 翻历史没问题。
;; 注意 ghostel-emacs-mode 现在是"切换"命令(已在 emacs 态时会退回上一个模式),
;; 所以这里必须先判一下状态,否则 hook 重复触发会把只读态切掉。inhibit-message 只为
;; 压掉每次进入时的提示。
(defun lakki/ghostel-meow-normal ()
  (unless (eq ghostel--input-mode 'emacs)
    (let ((inhibit-message t)) (ghostel-emacs-mode)))
  ;; ghostel 进只读态会把 cursor-type 设成 default,这里盖回 meow 的 normal 光标
  (meow--update-cursor))

;; INSERT 时只看是否在打字(semi-char 直通)。emacs-rime 的上屏转发交给
;; ghostel-ime-mode(见下)在 semi-char 下原生处理,不用再切 line-mode ——
;; 0.17 起 line-mode 在 alt-screen 的 raw TUI(Claude 这种)上只会"armed,
;; 等 TUI 退出才真正进入",强切没用,反而会让中文完全打不进去。
(defun lakki/ghostel-meow-insert ()
  (when (and (not current-input-method) (not (eq ghostel--input-mode 'semi-char)))
    (ghostel-semi-char-mode))
  (meow--update-cursor))

;; C-\ 只切 emacs-rime 本身;ghostel-ime-mode 负责把 rime 上屏的字转发进 PTY。
(defun lakki/ghostel-toggle-rime ()
  (interactive)
  (if current-input-method
      (deactivate-input-method)
    (activate-input-method "rime")))

(defun lakki/ghostel-meow-setup ()
  ;; 让 meow 独占光标:挡掉 ghostel 跟随终端 DEC 光标样式改 cursor-type;
  ;; normal/insert 切完模式后再 meow--update-cursor 重刷,光标就反映 meow 状态。
  (setq-local ghostel-ignore-cursor-change t)
  (add-hook 'meow-normal-mode-hook #'lakki/ghostel-meow-normal nil t)
  (add-hook 'meow-insert-mode-hook #'lakki/ghostel-meow-insert nil t))

(with-eval-after-load 'ghostel
  (define-key ghostel-semi-char-mode-map (kbd "C-\\") #'lakki/ghostel-toggle-rime)
  (define-key ghostel-line-mode-map (kbd "C-\\") #'lakki/ghostel-toggle-rime)
  (add-hook 'ghostel-mode-hook #'lakki/ghostel-meow-setup)
  ;; emacs-rime 是"插字进 buffer"式的 Lisp 输入法,semi-char 下默认不会转发进
  ;; PTY、还会被终端重绘抹掉。ghostel-ime-mode 包一层 input-method-function:
  ;; 上屏的字转发给 PTY,composing 期间让 ghostel 暂缓重绘,rime 就能直接在
  ;; semi-char 下用,不用再折腾 line-mode。
  (add-hook 'ghostel-mode-hook #'ghostel-ime-mode))

;; 中文输入(只用 emacs-rime,不碰系统输入法):在终端里按 C-\ 开/关 rime 即可,
;; semi-char 下直接打,ghostel-ime-mode 会把上屏的字转发进 PTY。line-mode 留给
;; 普通 shell 提示符续行编辑用,和中文输入已经无关。

;;; init-shell.el ends here
