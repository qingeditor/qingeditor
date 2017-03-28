;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(qingeditor/define-module
 php
 "The php config module"
 :has-extra-funcs-defs t
 :has-extra-config t
 :require-packages
 '(company
   company-php
   eldoc
   flycheck
   ggtags
   helm-gtags
   (php-extras :location (recipe :fetcher github :repo "arnested/php-extras"))
   php-mode
   phpcbf
   phpunit))
