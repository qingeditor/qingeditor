;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The load path provider

(defclass qingeditor/modulemgr/feature/load-path-provider ()
  ()
  :abstract
  :documentation "maybe this module has some private packages, so you
need load these packages and you must add these packages direcotry into
load path. If this module has this requirements, you can inherit this
interface.")

(defmethod qingeditor/cls/get-target-paths
  ((this qingeditor/modulemgr/feature/load-path-provider))
  "Returns the filenames to be loaded by module manager. Related to current
module direcotry."
  nil)

(provide 'qingeditor-modulemgr-load-path-provider)
