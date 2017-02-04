;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define some untils functions

(defun qingeditor/message (msg &rest args)
  "将`msg'添加到`*Messages*'buffer里面。"
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert (apply 'format (concat msg "\n") args)))))

(defun qingeditor/mplist-get (plist prop)
  "获取多值`mplist'指定属性的值，多值plist是一个关键字后面跟多个值。
当前的函数如果`circle list'那个会造成无限循环。如果相同的关键字间隔出现
我们只返回与第一个关键字关联的数据。"
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; 弹出关键字
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun qingeditor/mplist-remove (plist prop)
  "返回一个删除了指定关键字的关联数据。如果相同的关键字间隔出现，我们只删除第一个出现的关键字数据。"
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

;; 灵感来源 http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun qingeditor/dump-vars-to-file (varlist filename)
  "将变量列表`varlist'导出到文件`filename'。"
  (with-temp-file filename
    (qingeditor/core/dump varlist (current-buffer))
    (make-directory (file-name-directory filename) t)))

;; 灵感来源 http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun qingeditor/dump (varlist buffer)
  "在传入的`buffer'里面根据`varlist'传入的列表，生成设置语句，用于重新创建信息。"
  (cl-loop for var in varlist do
           (print (list 'setq var (list 'quote (symbol-value var)))
                  buffer)))

;; from https://gist.github.com/3402786
(defun qingeditor/toggle-maximize-buffer ()
  "最大化`buffer'。"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(defun qingeditor/run-text-mode-hooks ()
  "运行`text-mode-hook'。这个函数对那些不是从`text-mode'继承而来的`mode'非常有用。"
  (run-hooks 'text-mode-hook))

(defun qingeditor/system-is-mac ()
  (eq system-type 'darwin))

(defun qingeditor/system-is-linux ()
  (eq system-type 'gnu/linux))

(defun qingeditor/system-is-mswindows ()
  (eq system-type 'windows-nt))

(defun qingeditor/window-system-is-mac ()
  ;; 在Emacs 25+的mac `(window-system)'返回 ns
  (memq (window-system) '(mac ns)))

(defvar qingeditor/init-redisplay-count-private 0
  "The number of calls to `redisplay'.")

(defun qingeditor/redisplay ()
  "`redisplay' wrapper."
  (setq qingeditor/init-redisplay-count-private
        (1+ qingeditor/init-redisplay-count-private)))

(provide 'qingeditor-funcs)
