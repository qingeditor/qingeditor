;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The load path provider

(defclass qingeditor/modulemgr/feature/extra-files-loader ()
  ()
  :abstract t
  :documentation "Some module maybe want to loaded more files manually,
if you want to do this, you can inherit this interface instead. module
manager will help you to do this.")

(defmethod qingeditor/cls/get-target-filenames
  ((this qingeditor/modulemgr/feature/extra-files-loader))
  "Returns the filenames to be loaded by module manager. Related to current
module direcotry."
  nil)

(provide 'qingeditor-modulemgr-extra-files-loader)
