;;; init-shell.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; term + shell

(install-package 'ghostel)


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
;; ghostel-emacs-mode 自带幂等保护(已在 emacs 态时为 no-op),inhibit-message 只为
;; 压掉每次进入时的提示。
(defun lakki/ghostel-meow-normal ()
  (let ((inhibit-message t)) (ghostel-emacs-mode))
  ;; ghostel 进只读态会把 cursor-type 设成 default,这里盖回 meow 的 normal 光标
  (meow--update-cursor))

;; INSERT 时尊重 emacs-rime:rime 开着就进 line-mode(本地可编辑区,中文上屏留得住、
;; 回车整行发送),否则 semi-char(直通打字)。emacs-rime 在 semi-char/char 下不工作
;; (上屏的字直接插进 buffer、没进 PTY,会被重绘抹掉),所以中文只能走 line。
;; line 分支加 ghostel--term 守卫:pty 未起时 ghostel-line-mode 会 user-error。
(defun lakki/ghostel-meow-insert ()
  (cond
   ((and current-input-method ghostel--term (not (eq ghostel--input-mode 'line)))
    (ghostel-line-mode))
   ((and (not current-input-method) (not (eq ghostel--input-mode 'semi-char)))
    (ghostel-semi-char-mode)))
  (meow--update-cursor))

;; C-\ 切 emacs-rime + 同步切 ghostel 模式:开 rime→line-mode,关 rime→semi-char。
(defun lakki/ghostel-toggle-rime ()
  (interactive)
  (if current-input-method
      (progn (deactivate-input-method) (ghostel-semi-char-mode))
    (progn (activate-input-method "rime") (ghostel-line-mode))))

(defun lakki/ghostel-meow-setup ()
  ;; 让 meow 独占光标:挡掉 ghostel 跟随终端 DEC 光标样式改 cursor-type;
  ;; normal/insert 切完模式后再 meow--update-cursor 重刷,光标就反映 meow 状态。
  (setq-local ghostel-ignore-cursor-change t)
  (add-hook 'meow-normal-mode-hook #'lakki/ghostel-meow-normal nil t)
  (add-hook 'meow-insert-mode-hook #'lakki/ghostel-meow-insert nil t))

(with-eval-after-load 'ghostel
  (define-key ghostel-semi-char-mode-map (kbd "C-\\") #'lakki/ghostel-toggle-rime)
  (define-key ghostel-line-mode-map (kbd "C-\\") #'lakki/ghostel-toggle-rime)
  (add-hook 'ghostel-mode-hook #'lakki/ghostel-meow-setup))

;; 中文输入(只用 emacs-rime,不碰系统输入法):在终端里按 C-\ 开 rime 会切到
;; line-mode,在本地可编辑区用 rime 打字、回车整行发送;再 C-\ 关 rime 回 semi-char。
;; 注意:line-mode 是给行式 shell 设计的;Claude 这种会重画输入框的 TUI 里可能别扭
;; 甚至不工作(alt-screen 下 line-mode 会自动延后)。给 Claude 打中文若不顺,最稳是
;; 在别的 buffer 用 rime 写好再 C-y 粘进去(bracketed paste 能正确进 PTY)。

;;; init-shell.el ends here
