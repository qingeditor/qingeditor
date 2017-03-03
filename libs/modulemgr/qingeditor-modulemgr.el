;;; qingeditor --- a distribution of Emacs editor
;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;; Code:

(defvar qingeditor/modulemgr/mdoule-detect-hook nil
  "When module manager init, it will detect all the modules under
direcotry `qingeditor/modulemgr/module-directory'. when this stage finished,
run this hook.")

(defvar qingeditor/modulemgr/before-load-modules-hook nil
  "Before module manager begin load modules, run this hook.")

(defvar qingeditor/modulemgr/load-modules-hook nil
  "When module manager load modules, run this hook.")

(defvar qingeditor/modulemgr/load-module-resolve-hook nil
  "Before start load module cycle begin, we must get a module object,
run this hook.")

(defvar qingeditor/modulemgr/before-load-module-hook nil
  "Before module manager load target module,
run this hook.")

(defvar qingeditor/modulemgr/after-load-module-hook nil
  "After module manager finished load target module,
run this hook.")

(defvar qingeditor/modulemgr/before-install-packages-hook nil
  "Before install/update/uninstall packages, run this hook.")

(defvar qingeditor/modulemgr/before-install-package-hook nil
  "Before module manager install package, run this hook.")

(defvar qingeditor/modulemgr/install-package-hook nil
  "Before module manager install package, run this hook.")

(defvar qingeditor/modulemgr/after-install-package-hook nil
  "After module manager installed package, run this hook.")

(defvar qingeditor/modulemgr/before-configure-packages-hook nil
  "Before configure packages, run this hook.")

(defvar qingeditor/modulemgr/before-configure-packages-hook nil
  "Before configure package, run this hook.")

(defvar qingeditor/modulemgr/configure-packages-hook nil
  "When configure packages, run this hook.")

(defvar qingeditor/modulemgr/before-configure-packages-hook nil
  "Before configure packages, run this hook.")

(defvar qingeditor/modulemgr/after-configure-package-hook nil
  "After configure package, run this hook.")

(defun qingeditor/modulemgr/module-registered (module-name)
  nil)

(defun qingeditor/modulemgr/register-module (module-name)
  t)

(provide 'qingeditor-modulemgr)
