;;; init-elfeed.el --- RSS reader -*- no-byte-compile: t; lexical-binding: t; -*-

;; 订阅源来自 Folo 导出（2026-07-25）。每个源一个 tag，就是它在 Folo 里的
;; 分类，`elfeed-tree' 按 tag 分组，所以 `C-c F' 出来就是一棵按分类分好的树。
;;
;; 抓取在本地进行：rsshub.app / reddit / blogspot / huggingface /
;; cn.nytimes.com 这几个源被墙时，用 `elfeed-curl-extra-arguments' 配代理。

(install-package 'elfeed)

(setq elfeed-search-filter "@2-weeks-ago +unread")

;; 首次抓取时各家的历史文章会一起涌进来，一个月以前的直接标已读。
(with-eval-after-load 'elfeed
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "1 month ago" :remove 'unread)))

(keymap-global-set "C-c f" #'elfeed)
(keymap-global-set "C-c F" #'elfeed-tree)

(setq elfeed-feeds
      '(
        ;; blog -- 个人博客
        ("https://ttys3.dev/feed.xml" blog)                          ; /dev/ttyS3
        ("https://antonz.org/index.xml" blog)                        ; Anton Zhiyanov
        ("https://lemire.me/blog/feed/" blog)                        ; Daniel Lemire's blog
        ("https://blog.fal.ai/rss/" blog)                            ; fal
        ("https://frame.work/ie/en/blog.rss" blog)                   ; framework-latop
        ("https://hackerstations.com/index.xml" blog)                ; Hacker Stations
        ("https://jdhao.github.io/index.xml" blog)                   ; jdhao
        ("https://lantian.pub/feed.xml" blog)                        ; Lan Tian @ Blog
        ("https://michael.stapelberg.ch/feed.xml" blog)              ; Michael Stapelbergs Website
        ("https://nixcademy.com/feed.xml" blog)                      ; Nixcademy Blog
        ("https://opensource.com/feed" blog)                         ; opensource
        ("https://www.piglei.com/feeds/latest/" blog)                ; piglei
        ("https://jcs.org/rss" blog)                                 ; rubewarden
        ("https://silaoa.github.io/atom.xml" blog)                   ; silaoA的博客
        ("https://simonwillison.net/atom/everything/" blog)          ; Simon Willison's Weblog
        ("https://willvaughn.org/index.xml" blog)                    ; some org roam
        ("https://blog.skk.moe/atom.xml" blog)                       ; Sukka's Blog
        ("https://system76.com/blog/rss.xml" blog)                   ; System76 Blog RSS Feed
        ("https://determinate.systems/rss.xml" blog)                 ; The Determinate Systems blog
        ("https://thiscute.world/index.xml" blog)                    ; This Cute World
        ("https://lo-li.cn/feed" blog)                               ; ZeroDream
        ("https://rsshub.app/bulianglin" blog)                       ; 不良林
        ("https://blog.lilydjwg.me/feed?code=c6f56c4214621ab98b86acbcae6b4405" blog) ; 依云
        ("https://rsshub.app/juejin/posts/2295436008498765" blog)    ; 掘金专栏-dragonir
        ("https://www.bmpi.dev/index.xml" blog)                      ; 构建我的被动收入
        ("https://program-think.blogspot.com/feeds/posts/default?max-results=5&redirect=false" blog) ; 编程随想
        ("https://blog.ops-coffee.com/feed.xml" blog)                ; 运维咖啡吧
        ("https://coolshell.cn/feed" blog)                           ; 酷 壳 – CoolShell
        ("https://blog.mountaye.com/feed.xml" blog)                  ; 阿掖山：一个博客

        ;; emacs
        ("https://coredumped.dev/index.xml" emacs)                ; Core Dumped
        ("https://www.emacs.dyerdwelling.family/index.xml" emacs) ; DyerDwelling Emacs
        ("https://lmno.lol/alvaro/feed" emacs)                    ; Emacs Mac
        ("https://emacsnotes.wordpress.com/feed/" emacs)          ; Emacs Notes
        ("https://emacsredux.com/atom.xml" emacs)                 ; Emacs Redux
        ("https://egh0bww1.com/rss.xml" emacs)                    ; include-yy's blog
        ("https://manateelazycat.github.io/feed.xml" emacs)       ; Lazycat
        ("https://www.reddit.com/r/emacs/.rss" emacs)             ; M-x emacs-reddit
        ("https://tilde.town/~ramin_hal9001/atom.xml" emacs)      ; Ramin Honary's Blog
        ("https://emacstil.com/feed.xml" emacs)                   ; Tody I Learned Emacs
        ("https://sh.alynx.one/atom.xml" emacs)                   ; 喵's StackHarbor
        ("https://zilongshanren.com/index.xml" emacs)             ; 子龙山人

        ;; golang
        ("https://dave.cheney.net/feed" golang)                      ; Dave Cheney
        ("https://halfrost.com/rss/" golang)                         ; Halfrost's Field | 冰霜之地
        ("https://threedots.tech/index.xml" golang)                  ; Learn Building Modern Go applications
        ("https://xargin.com/rss/" golang)                           ; No Headback
        ("https://rakyll.org/index.xml" golang)                      ; rakyll
        ("https://research.swtch.com/feed.atom" golang)              ; research!rsc
        ("https://www.blogger.com/feeds/6983287/posts/default" golang) ; Rob Pike
        ("https://divan.dev/index.xml" golang)                       ; Visualizing Concurrency in Go
        ("https://eddycjy.com/posts/index.xml" golang)               ; 煎鱼  <- 证书已过期，curl 会拒绝
        ("https://www.flysnow.org/index.xml" golang)                 ; 飞雪无情

        ;; ai
        ("https://rsshub.app/anthropic/news" ai)             ; Anthropic News
        ("https://qwenlm.github.io/zh/blog/index.xml" ai)    ; Blog on Qwen
        ("https://blog.comfy.org/feed" ai)                   ; ComfyUI Blog
        ("https://huggingface.co/blog/feed.xml" ai)          ; Hugging Face - Blog
        ("https://blog.jetbrains.com/ai/feed/" ai)           ; JetBrains AI
        ("https://lastweekin.ai/feed" ai)                    ; Last Week in AI
        ("https://stability.ai/news?format=rss" ai)          ; News - Stability AI
        ("https://www.reddit.com/r/StableDiffusion/.rss" ai) ; StableDiffusion

        ;; daily -- 日更/周更
        ("https://sachachua.com/blog/category/emacs-news/feed/atom/" daily) ; Emacs News Weekly
        ("https://rss.zslren.com/mp/feed/63a622684135a067fc1e13f50142d2b8f231ff8d.xml" daily) ; FLIGHTCLUB中文站
        ("https://www.daemonology.net/hn-daily/index.rss" daily)     ; Hacker News Daily
        ("https://sspai.com/feed" daily)                             ; 少数派
        ("https://feeds.feedburner.com/ruanyifeng" daily)            ; 阮一峰的网络日志

        ;; company -- 公司/产品博客
        ("https://blogs.nvidia.com/feed/" company)        ; NVIDIA Blog
        ("https://stackoverflow.blog/feed/" company)      ; Stack Overflow Blog
        ("https://blog.cloudflare.com/rss/" company)      ; The Cloudflare Blog
        ("https://blog.jetbrains.com/feed/" company)      ; The JetBrains Blog
        ("https://victoriametrics.com/index.xml" company) ; VictoriaMetrics: Simple & Reliable Monitoring for Everyone on VictoriaMetrics

        ;; game
        ("https://rsshub.app/rsshub/transform/html/https%3A%2F%2Fwww.wlgooo.com%2Fhanhua/title%3D%E5%9B%B4%E7%82%89%20%E6%96%B0%E6%B1%89%E5%8C%96%26item%3D.upk-timeline-title%26itemTitle%3Da%26itemLink%3Da%26itemDesc%3D.upk-timeline-desc%26itemPubDate%3D.upk-date%26itemContent%3D%23yueduye" game) ; 围炉 新汉化
        ("https://www.yystv.cn/rss/feed" game)                       ; 游研社

        ;; misc -- Folo 里的「未分类」
        ("http://arthurchiao.art/feed.xml" misc)              ; ArthurChiao's Blog
        ("https://rsshub.app/bbc/chinese" misc)               ; BBC中文
        ("https://drakerossman.com/feed.xml" misc)            ; Drake Rossman's Website
        ("https://karthinks.com/index.xml" misc)              ; Karthinks
        ("https://linux.cn/rss.xml" misc)                     ; Linux 中国
        ("https://lobste.rs/rss" misc)                        ; Lobsters
        ("https://lwn.net/headlines/newrss" misc)             ; LWN
        ("https://www.stardewvalley.net/feed/" misc)          ; Stardew Valley
        ("https://statmodeling.stat.columbia.edu/feed/" misc) ; Statistical Modeling, Causal Inference, and Social Science
        ("https://xeiaso.net/blog.rss" misc)                  ; Xe's Blog
        ("https://xkcd.com/rss.xml" misc)                     ; xkcd
        ("https://feed.iplaysoft.com/" misc)                  ; 异次元软件世界
        ("https://www.williamlong.info/rss.xml" misc)         ; 月光博客
        ("https://shadiao.plus/feed" misc)                    ; 沙雕普拉斯 | 沙雕新闻
        ("https://cn.nytimes.com/rss/" misc)                  ; 纽约时报中文网
        ("https://hutusi.com/feed.xml" misc)                  ; 胡涂说
        ))

;;; init-elfeed.el ends here
