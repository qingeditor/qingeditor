;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The emacs setup listener, The duty of this listener
;; is setup `emacs' UI elements, setup some global variable
;; and load `emacs' theme.

(require 'qingeditor-eventmgr-listener-aggregate)

(defclass qingeditor/init/emacs-setup-listener
  (qingeditor/eventmgr/listener-aggregate)
  ()
  :documentation "The emacs setup listene")

(defmethod qingeditor/init/emacs-setup-listener/attach
  ((this qingeditor/init/emacs-setup-listener))
  )


