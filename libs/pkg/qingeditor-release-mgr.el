;; `qingeditor'发布管理

(require 'qingeditor-std-dir)

(defvar qingeditor/pkg/release-mgr/repo-name "qingeditor"
  "`qingeditor'的发行软件包的名称。")

(defvar qingeditor/pkg/release-mgr/repo-owner "qcoreteam"
  "`qingeditor'的github库的所有者。")

(defvar qingeditor/pkg/release-mgr/checkversion-remote-name "checkversion"
  "用于检查版本号的远端库的名称。")

(defvar qingeditor/pkg/release-mgr/checkversion-repo-branch "master"
  "用与检查版本号的git库的分支名称。")

(defvar qingeditor/pkg/release-mgr/new-version nil
  "当前可用的`qingeditor'的版本号。")

(defvar qingeditor/pkg/release-mgr/version-check-interval nil
  "两次版本检查之间的间隔时间。")

(defvar qingeditor/pkg/release-mgr/version-check-lighter "[+]"
  "当有新版本提供的时候，这个小标志将在mode-line上面进行显示。")

(defvar qingeditor/pkg/release-mgr/version-check-timer-private nil
  "当前用于检查版本更新的`timer'对象的引用。")

(defvar qingeditor/pkg/release-mgr/version-last-startup-check-file-private
  (expand-file-name (concat qingeditor/cache-dir "last-version-check"))
  "这个文件主要存放最后检查版本号的时间。")

(defvar qingeditor/pkg/release-mgr/version-last-startup-check-time-private nil
  "最后检查版本号时间。")

;; 一天检查一次是否有新版本
(defvar qingeditor/pkg/release-mgr/version-last-startup-check-interval-private (* 3600 24)
  "检查版本号的间隔。")

(defun qingeditor/pkg/release-mgr/check-for-new-version (force &optional interval)
  "间歇性去检查`qingeditor'是否有新的版本。"
  (cond
   ((and (not force)
         (not qingeditor/core/user-cfg/check-for-update))
    (qingeditor/core/message "跳过新版本的检查，原因: 配置文件禁止检查。"))
   ((and (not force)
         (string-equal "develop" (qingeditor/pkg/release-mgr/get-current-branch)))
    (qingeditor/core/message "跳过新版本的检查，原因：当前在develop分支。"))
   ((and (not force)
         (not (qingeditor/pkg/release-mgr/can-check-for-new-version-at-startup)))
    (message "跳过新版本的检查，原因：最近一次检查还没有过期，不需要检查。"))
   ((require 'async nil t)
    (qingeditor/core/message "开始检查新版本...")
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`qingeditor/start-dir\\'")
        (load-file (expand-file-name (concat user-emacs-directory "init-autoload.el")))
        (qingeditor/register-target-dir-to-load-path qingeditor/libs-dir)
        (require 'eieio)
        (require 'qingeditor-std-dir)
        (require 'qingeditor-release-mgr)
        (qingeditor/pkg/release-mgr/get-last-version))
     (lambda (result)
       (if result
           (if (or (version< result qingeditor-version)
                   (version= result qingeditor-version)
                   (if qingeditor/pkg/release-mgr/new-version
                       (string= result qingeditor/pkg/release-mgr/new-version)))
               (qingeditor/core/message "qingeditor已经是最新版。")
             (qingeditor/core/message "发现qingeditor新版本：%s" result)
             qingeditor/pkg/release-mgr/new-version result)
         (qingeditor/core/message "检查新版本出错。"))))
    (when interval
      (setq qingeditor/pkg/release-mgr/version-check-timer-private
            (run-at-time t (timer-duration interval)
                                 (lambda ()
                                   (qingeditor/pkg/release-mgr/check-for-new-version nil))))))))

(defun qingeditor/pkg/release-mgr/get-last-version ()
  "获取github官网库的最新版本。"
  (qingeditor/pkg/release-mgr/do-get-last-version
   qingeditor/pkg/release-mgr/repo-name
   qingeditor/pkg/release-mgr/repo-owner
   qingeditor/pkg/release-mgr/checkversion-remote-name
   qingeditor/pkg/release-mgr/checkversion-repo-branch))

(defun qingeditor/pkg/release-mgr/do-get-last-version (repo owner remote branch)
  "获取github官网库的最新版本。"
  (let ((url (format "https://www.github.com/%s/%s" owner repo)))
    (qingeditor/pkg/release-mgr/git-remove-remote remote)
    (qingeditor/pkg/release-mgr/git-add-remote remote url)
    (qingeditor/pkg/release-mgr/git-fetch-tags remote branch))
  (let ((version (qingeditor/pkg/release-mgr/get-latest-tag remote branch)))
    (when version
      (save-match-data
        (string-match "^.*\\([0-9]+\\.[0-9]+\\.[0-9]+\\)$" version)
        (match-string 1 version)))))

(defun qingeditor/pkg/release-mgr/get-latest-tag (remote branch)
  "获取最新的版本信息。"
  (let ((proc-buffer "git-get-latest-tag")
        (default-directory (file-truename qingeditor/start-dir))
        (where (format "%s/%s" remote branch)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "describe" "--tags" "--abbrev=0"
                              "--match=v*" where "FETCH_HEAD"))
      (with-current-buffer proc-buffer
        (prog1
            (when (buffer-name)
              (goto-char (point-max))
              (forward-line -1)
              (replace-regexp-in-string "\n$" ""
                                        (buffer-substring (line-beginning-position)
                                                          (line-end-position))))
          (kill-buffer proc-buffer))))))

(defun qingeditor/pkg/release-mgr/git-fetch-tags (remote branch)
  "从远端库`remote'的`branch'分支拉取所有的`tags'。"
  (let ((proc-buffer "git-fetch-tags")
        (default-directory (file-truename qingeditor/start-dir)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" "--tags" remote branch))
      (kill-buffer proc-buffer))))

(defun qingeditor/pkg/release-mgr/git-has-remote (remote)
  "如果软件库`remote'定义了返回`t'。"
  (let ((proc-buffer "git-has-remote")
        (default-directory (file-truename qingeditor/start-dir)))
    (when (eq 0 (process-file "git" nil proc-buffer nil "remote"))
      (prog2
          (goto-char (point-min))
          (re-search-forward (format "^%s$" remote) nil 'noerror)
        (kill-buffer proc-buffer)))))

(defun qingeditor/pkg/release-mgr/git-remove-remote (remote)
  "删除一个git库远端，如果成功返回`t'。"
  (let ((proc-buffer "git-remove-remote")
        (default-directory (file-truename qingeditor/start-dir)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "remote" "remove" remote))
      (kill-buffer proc-buffer))))

(defun qingeditor/pkg/release-mgr/git-add-remote (remote url)
  "添加一个git库远端，如果成功返回`t'。"
  (let ((proc-buffer "git-add-remote")
        (default-directory (file-truename qingeditor/start-dir)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "remote" "add" remote url))
      (kill-buffer))))

(defun qingeditor/pkg/release-mgr/git-fetch-remote (remote)
  "获取指定的`remote'的远端库。"
  (let ((proc-buffer "git-fetch-remote")
        (default-directory (file-truename qingeditor/core/start-dir)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" remote))
      (kill-buffer proc-buffer))))

(defun qingeditor/pkg/release-mgr/git-hard-reset-to-tag (tag)
  "重置当前的分支到指定的`tag'。"
  (let ((proc-buffer "git-hard-reset")
        (default-directory (file-truename qingeditor/start-dir)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "reset" "--hard" tag))
      (kill-buffer proc-buffer))))

(defun qingeditor/pkg/release-mgr/git-checkout (branch)
  "检出指定的`branch'分支，如果检出成功返回`t'。"
  (let ((proc-buffer "git-checkout")
        (default-directory (file-truename qingeditor/start-dir)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "checkout" branch))
      (kill-buffer proc-buffer))))

(defun qingeditor/pkg/release-mgr/get-current-branch ()
  "获取当前的特性分支，如果有错误发生返回`nil'。"
  (let ((proc-buffer "git-get-current-branch")
        (default-directory (file-truename qingeditor/start-dir)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "symbolic-ref" "--short" "-q" "HEAD"))
      (with-current-buffer proc-buffer
        (prog1
            (when (buffer-string)
              (goto-char (point-min))
              (replace-regexp-in-string
               "\n$" ""
               (buffer-substring (line-beginning-position)
                                 (line-end-position))))
          (kill-buffer proc-buffer))))))

(defun qingeditor/pkg/release-mgr/working-directory-dirty ()
  "如果工作空间有改动，函数返回`nil'，根据`git'的`--porcelain'参数进行判断。"
  (let ((proc-buffer "git-working-directory-dirty")
        (default-directory (file-truename qingeditor/start-dir)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "status" "--porcelain"))
      (with-current-buffer proc-buffer
        (prog1
            (when (and (buffer-string)
                       ;; 只要有任何的输出就是已经改动
                       (string-match-p "[^ \t\n]" (buffer-string)))
              (replace-regexp-in-string  "\n\\'" "" (buffer-string)))
          (kill-buffer proc-buffer))))))

(defun qingeditor/pkg/release-mgr/can-check-for-new-version-at-startup ()
  "如果返回不为`nil'那么我们将在启动的时候检查是否存在新版本。"
  (when (file-exists-p qingeditor/pkg/release-mgr/version-last-startup-check-file-private)
    (load qingeditor/pkg/release-mgr/version-last-startup-check-file-private))
  (let ((result
         (or (null qingeditor/pkg/release-mgr/version-last-startup-check-time-private)
             (> (- (float-time) qingeditor/pkg/release-mgr/version-last-startup-check-time-private)
                qingeditor/pkg/release-mgr/version-last-startup-check-interval-private))))
    (when result
      (setq qingeditor/pkg/release-mgr/version-last-startup-check-time-private
	    (float-time))
      (qingeditor/core/dump-vars-to-file '(qingeditor/pkg/release-mgr/version-last-startup-check-time-private)
					 qingeditor/pkg/release-mgr/version-last-startup-check-file-private))
    result))

(provide 'qingeditor-release-mgr)
