;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The dependency indicator interface

(defclass qingeditor/modulemgr/feature/dependency-indicator ()
  ()
  :abstract t
  :documentation "The dependency indicator interface.
After module load the dependencies of current module, you can
inherit this interface to confirm required modules is already loaded.")

(defmethod qingeditor/cls/get-module-dependencies
  ((this qingeditor/modulemgr/feature/dependency-indicator))
  "This method should return the names of dependencies."
  nil)

(provide 'qingeditor-modulemgr-dependency-indicator)
