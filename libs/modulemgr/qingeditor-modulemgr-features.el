;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; This file define the features that module manager supported

(defclass qingeditor/modulemgr/feature/require-modules ()
  ()
  :abstract t
  :documentaion " The require modules feature

If this module dependent on some others modules, you can
inherit this feature interface.")

(defmethod qingeditor/cls/get-require-modules
  ((this qingeditor/modulemgr/feature/require-modules))
  "If this module dependent on some other modules, you can return
a list of dependent modules symbol."
  nil)

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

(defclass qingeditor/modulemgr/feature/load-path-provider ()
  ()
  :abstract t
  :documentation "The load path provider

maybe this module has some private packages, so you
need load these packages and you must add these packages direcotry into
load path. If this module has this requirements, you can inherit this
interface.")

(defmethod qingeditor/cls/get-load-paths
  ((this qingeditor/modulemgr/feature/load-path-provider))
  "Returns the filenames to be loaded by module manager. Related to current
module direcotry."
  nil)

(defclass qingeditor/modulemgr/feature/run-after-editor-ready ()
  ()
  :abstract t
  :documentation "The run after editor init ready interface.

If you have some function need to be invoked after `qingeditor' initialized.
In this method you can return a list of current module's method symbol. If
you must ensure the invoked order, you can return a list like this:
`((,#'method-symbol1 1) (,#'method-symbol2 2)).
if you don't care about invoke order, you can just return:
(list #'method-symbol1 #'method-symbol2).")

(defmethod qingeditor/cls/get-callables ((this qingeditor/modulemgr/feature/run-after-editor-ready))
  "return callables list that want to be invoked after `qingeditor' initialized.
you can return list like:
`((,#'method-symbol1 1) (,#'method-symbol2 2))
or
(list #'method-symbol1 #'method-symbol2)."
  nil)

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
 
(provide 'qingeditor-modulemgr-features)
