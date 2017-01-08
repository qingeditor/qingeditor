;;; evil-evilified-state.el --- A minimalistic evil state
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil spacemacs
;; Created: 22 Mar 2015
;; Version: 1.0
;; Package-Requires: ((evil "1.0.9"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Define a `evilified' evil state inheriting from `emacs' state and
;; setting a minimalist list of Vim key bindings (like navigation, search, ...)

;; The shadowed original mode key bindings are automatically reassigned
;; following a set of rules:
;; Keys such as 
;; /,:,h,j,k,l,n,N,v,V,gg,G,C-f,C-b,C-d,C-e,C-u,C-y and C-z 
;; are working as in Evil.
;; Other keys will be moved according to this pattern:
;; a -> A -> C-a -> C-A
;; The first unreserved key will be used. 
;; There is an exception for g, which will be directly
;; bound to C-G, since G and C-g (latest being an important escape key in Emacs) 
;; are already being used.

;;; Code:

(require 'evil)
(require 'bind-map)

(defvar qingeditor/editor-base/evilified-state/evil-surround nil
  "Evil surround模式变量缓存。")
(make-variable-buffer-local 'qingeditor/editor-base/evilified-state/evil-surround)

(defvar qingeditor/editor-base/evilified-state/normal-state-map nil
  "本地针对`normal-state-map'缓存。")
(make-variable-buffer-local 'qingeditor/editor-base/evilified-state/normal-state-map)

(evil-define-state
 qingeditor-evilified
 "Evilified state.
小心的选择了一些`vim'的按键绑定与`emacs state'混合"
 :tag " <N'> "
 :enable (emacs)
 :message "-- EVILIFIED BUFFER --"
 :cursor box)

(provide 'evil-evilified-state)
