;; 定义`qingeditor'标准的文件目录

(defconst qingeditor/assets-dir
  (expand-file-name (concat qingeditor/start-dir "assets/"))
  "文件存放目录，主要是存放一些图片，文档等等文件的文件夹目录.")

(defconst qingeditor/info-dir
  (expand-file-name (concat qingeditor/assets-dir "info/"))
  "`qingeditor'info相关文件存放文件夹.")

(defconst qingeditor/release-notes-dir
  (expand-file-name (concat qingeditor/info-dir "release-notes/"))
  "`qingeditor'版本发布相关日志信息存放文件夹.")

(defconst qingeditor/template-dir
  (expand-file-name (concat qingeditor/assets-dir "templates/"))
  "`qingeditor'默认的配置信息模板.")

(defconst qingeditor/banner-dir
  (expand-file-name (concat qingeditor/assets-dir "banners/"))
  "`qingeditor'banner文件存放文件夹.")

(defconst qingeditor/cache-dir
  (expand-file-name (concat qingeditor/start-dir ".cache/"))
  "`qingeditor'缓存文件夹.")

(defconst qingeditor/auto-save-dir
  (expand-file-name (concat qingeditor/cache-dir "auto-save/"))
  "`qingeditor'自动保存文件数据时候存放的文件夹.")

(defconst qingeditor/docs-dir
  (expand-file-name (concat qingeditor/start-dir "docs/"))
  "`qingeditor'一些文档存放的文件夹.")

(defconst qingeditor/news-dir
  (expand-file-name (concat qingeditor/start-dir "news/"))
  "`qingeditor'官方新闻文件存放文件夹.")

(defconst qingeditor/user-home-dir
  (expand-file-name "~/")
  "当前用户的home文件夹.")

(defconst qingeditor/pcache-dir (concat qingeditor/cache-dir "pcache/")
  "`qingeditor'持久化数据存放缓存文件夹.")

(unless (file-exists-p qingeditor/cache-dir)
  (make-directory qingeditor/cache-dir))

(provide 'qingeditor-stddir)
