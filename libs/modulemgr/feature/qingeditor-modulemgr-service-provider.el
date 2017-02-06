;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; Define service manager provider feature

(defclass qingeditor/modulemgr/feature/service-provider ()
  ()
  :abstract t
  :documentation "service manager provider feature.
If module want to register some object to global service manager
can inherit this interface.")

(defmethod qingeditor/cls/register-service
  ((this qingeditor/modulemgr/feature/service-provider)
   servicemgr)
  t)

(provide 'qingeditor-modulemgr-service-provider)
