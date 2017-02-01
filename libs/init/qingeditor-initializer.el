;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define global initializer class
(require 'eieio-base)

(defclass qingeditor/initializer (eieio-singleton)
  ()
  :documentation "global initializer class")

(defmethod qingeditor/initializer/init ((this qingeditor/initializer))
  "init `qingeditor' in this method, we first find the configuration
file in load-path, if the configuration not exist, `qingeditor' will
first generate it, then load it normally. after load the configuration file,
we finally process `qingeditor' modules."
  (qingeditor/initializer/init/load-editor-cfg-file this)
  ()
  )

(defmethod qingeditor/initializer/load-editor-cfg-file
  ((this qingeditor/initializer))
  "load `qingeditor' configuration file."
  (let ((target-cfg-filename (qingeditor/initializer/detect-init-filename this)))
    ))

(defmethod qingeditor/initializer/detect-init-filename
  ((this qingeditor/initializer))
  "figure out the target configuration full filename."
  )

(provide 'qingeditor-initializer)
