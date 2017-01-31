;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;; 
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;; 
;; 封装一个事件结果集合对象

(require 'qingeditor/stack)

(defclass qingeditor/eventmgr/response-collection (qingeditor/stack)
  ()
  :documentation "事件派发结果栈。")
