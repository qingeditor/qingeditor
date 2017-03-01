;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; Define which-key keymap prefixes
(setq qingeditor/editor-base/key-binding-prefixes
      '(("a"    "applications")
        ("ai"   "irc")
        ("as"   "shells")
        ("b"    "buffers")
        ("c"    "compile/comments")
        ("C"    "capture/colors")
        ("e"    "errors")
        ("f"    "files")
        ("fC"   "files/convert")
        ("fe"   "emacs(qingeditor)")
        ("fv"   "variables")
        ("g"    "git/version-control")
        ("h"    "help")
        ("hd"   "help-describe")
        ("i"    "insertion")
        ("j"    "jump/join/split")
        ("k"    "lisp")
        ("kd"   "delete")
        ("kD"   "delete-backward")
        ("n"    "narrow/numbers")
        ("p"    "projects")
        ("p$"   "projects/shell")
        ("q"    "quit")
        ("r"    "registers/rings/resume")
        ("Re"   "elisp")
        ("Rp"   "pcre")
        ("s"    "search/symbol")
        ("sa"   "ag")
        ("sg"   "grep")
        ("sk"   "ack")
        ("st"   "pt")
        ("sw"   "web")
        ("t"    "toggles")
        ("tC"   "colors")
        ("th"   "highlight")
        ("tm"   "modeline")
        ("T"    "UI toggles/themes")
        ("C-t"  "other toggles")
        ("w"    "windows")
        ("wp"   "popup")
        ("x"    "text")
        ("xa"   "align")
        ("xd"   "delete")
        ("xg"   "google-tanslate")
        ("xl"   "lines")
        ("xm"   "move")
        ("xt"   "transpose")
        ("xw"   "words")
        ("z"    "zoom")))

(mapc (lambda (x) (apply #'qingeditor/key-binder/declare-prefix x))
      qingeditor/editor-base/key-binding-prefixes)

