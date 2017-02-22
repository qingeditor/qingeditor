;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define `qingedtior' global configuration variables

(defvar qingeditor/config/target-cfg-dir nil
  "The target runtime environment dir, which the actually configuration
file to be saved.")

(defvar qingeditor/config/target-cfg-filename nil
  "The actually configuration to be load.")

(defvar qingeditor/config/distribution 'editor-standard
  "Base distribution to use. This is a module contained in the directory
`+distributions'. For now available distributions are `editor-base' or `editor-standard'.")

(defvar qingeditor/config/elpa-https t
  "If non `nil' `ELPA' repositories are contacted via HTTPS whenever it's
possible. Set it to `nil' if you have no way to use HTTPS in your
environment, otherwise it is strongly recommended to let it set to it.")

(defvar qingeditor/config/elpa-timeout 5
  "Maximum allowed time in seconds to contact an ELPA repository.")

(defvar qingeditor/config/elpa-subdirectory nil
  "If non-nil, a form that evaluates to a package directory. For example,
to use different package directories for different Emacs
versions, set this to `emacs-version'.")

(defvar qingeditor/config/cfg-module-dir '()
  "List of additonal paths where to look for configuration modules.
Paths must have a trailing slash (ie. `~/somedir/')")

(defvar qingeditor/config/install-packages 'used-only
  "Defines the behavior of `qingeditor' when installing packages.
Possible values are `used-only', `used-but-keep-unused' and `all'. `used-only'
installs only explicitly used packages and uninstall any unused packages as well
as their unused dependencies.`used-but-keep-unused' install only the used packages
but won't uninstall theme if they become unused. `all' installs *all*
packages supported by`qingeditor' and never uninstall them.")

(defvar qingeditor/config/lazy-installation-type 'unused
  "Lazy installation of modules (i.e. modules are installed only when a file
with a supported type is opened.) Possible values are `all', `unused' and `nil'.
`unused' will lazy install only unused modules (i.e. modules not listed in
variable `qingeditor/config/configuration-modules'), `all' will lazy install any
module that support lazy installation even the modules listed in
`qingeditor/config/configuration-modules'. `nil' disabled the lazy installation feature
and you have explicitly list a module in the variable `qingeditor/config/configuration-modules'
to install it.")

(defvar qingeditor/config/ask-for-lazy-installation t
  "If non-nil then `qingeditor' will ask for confirmation before installing
a module lazily.")

(defvar qingeditor/config/additional-packages '()
  "List of additional packages that will be installed without being
wrapped in a module. if you need some configuration for these
packages then consider to create a layer, you can also put
the configuration in `qingeditor/config/user-config-setup'")

(defvar qingeditor/config/startup-banner 'official
  "Specify the startup banner. Default value is `offical', it displays
the offical `qingeditor' logo. An integer value is the index of text
banner, `random' chooses a random text banner in `assets/banners'
directory. A string value must be a path to a .PNG file.
If the value is nil then no banner is displayed.")

(defvar qingeditor/config/scratch-mode 'text-mode
  "Default major mode of the scratch buffer.")

(defvar qingeditor/config/check-for-update nil
  "If non `nil' then `qingeditor' will check for updates at startup
when the current branch is not `develop'. Note that checking for
new versions works via git commands, thus it calls Github services
whenever you start Emacs.")

(defvar qingeditor/config/configuration-modules '(emacs-lisp)
  "List of configuration modules to load.")

(defvar qingeditor/config/configuration-modules-saved nil
  "Saved value of `qingeditor/config/configuration-modules' after sync.")

(defvar qingeditor/config/themes '(spacemacs-dark spacemacs-light)
  "List of themes, the first of the list is loaded when `qingeditor' starts.
Press `SPC T n' to cycle to the next theme in the list (works great
with 2 theme variables, one dark and one light.)")

(defvar qingeditor/config/colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color in GUI Emacs.")

(defvar qingeditor/config/leader-key "M-m"
  "The leader key.")

(defvar qingeditor/config/major-mode-leader-key "C-M-m"
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m'. Set it to `nil' to disable it.")

(defvar qingeditor/config/command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing on the leader key).")

(defvar qingeditor/config/distinguish-gui-tab nil
  "These variables control whether separate commands are bound in the GUI to
the key pairs C-i, TAB and C-m, RET.
Setting it to a non-nil value, allows for separate commands under <C-i>
and TAB or <C-m> and RET.
In the terminal, these pairs are generally indistinguishable, so this only
works in the GUI. (default nil)")

(defvar qingeditor/config/distinguish-gui-ret nil
  "If non `nil', distinguish `C-i' and tab in the GUI version of emacs.")

(defvar qingeditor/config/default-font
  '("Source Code Pro"
    :size 15
    :weight normal
    :width normal
    :powerline-scale 1.1)
  "Default font, or prioritized list of fonts. `powerline-scale'
allows to quickly tweak the mode-line size to make separators
look not too crappy.")

(defvar qingeditor/config/folding-method 'origami
  "Code folding method. Possible values is `origami'.")

(defvar qingeditor/config/default-layout-name "Default"
  "Name of the default layout.")

(defvar qingeditor/config/display-default-layout nil
  "If non nil the default layout name is displayed in the mode-line.")

(defvar qingeditor/config/auto-resume-layouts nil
  "If non nil then the last auto saved layouts are resume automatically upon
start.")

(defvar qingeditor/config/max-rollback-slots 5
  "Maximum number of rollback slots to keep in the cache.")

(defvar qingeditor/config/helm-resize nil
  "If non nil, `helm' will try to minimize the space it uses.")

(defvar qingeditor/config/helm-no-header nil
  "If non nil, the helm header is hidden when there is only one source.")

(defvar qingeditor/config/helm-position 'bottom
  "Position in which to show the `helm' mini-buffer.")

(defvar qingeditor/config/helm-use-fuzzy 'always
  "Controls fuzzy matching in helm. If set to `always', force fuzzy matching
  in all non-asynchronous sources. If set to `source', preserve individual
  source settings. Else, disable fuzzy matching in all sources.")

(defvar qingeditor/config/large-file-size 2
  "Size (in MB) above which spacemacs will prompt to open the large file
literally to avoid performance issues. Opening a file literally means that
no major mode or minor modes are active.")

(defvar qingeditor/config/auto-save-file-location 'cache
  "Location where to auto-save files. Possible values are `original' to
auto-save the file in-place, `cache' to auto-save the file to another
file stored in the cache directory and `nil' to disable auto-saving.
Default value is `cache'.")

(defvar qingeditor/config/enable-paste-transient-state t
  "If non nil the paste transient-state is enabled. While enabled pressing `p'
several times cycle between the kill ring content.")

(defvar qingeditor/config/which-key-delay 0.4
  "Delay in seconds starting from the last keystroke after which
the which-key buffer will be shown if you have not completed a
key sequence. Setting this variable is equivalent to setting
`which-key-idle-delay'.")

(defvar qingeditor/config/which-key-position 'bottom
  "Location of the which-key popup buffer. Possible choices are bottom,
right, and right-then-bottom. The last one will display on the
right if possible and fallback to bottom if not.")

(defvar qingeditor/config/loading-progress-bar t
  "If non nil a progress bar is displayed when `qingeditor' is loading. This
may increase the boot time on some systems and emacs builds, set it to nil
to boost the loading time.")

(defvar qingeditor/config/fullscreen-at-startup nil
  "If non nil the frame is fullscreen when Emacs starts up (Emacs 24.4+ only).")

(defvar qingeditor/config/fullssreen-use-non-native nil
  "If non nil `qingeditor/toggle-fullscreen' will not use native fullscreen. Use
to disable fullscreen animations in OSX.")

(defvar qingeditor/config/maximized-at-startup nil
  "If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).
Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.")

(defvar qingeditor/config/active-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'.")

(defvar qingeditor/config/inactive-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'.")

(defvar qingeditor/config/show-transient-state-title t
  "If non nil show the titles of transient states.")

(defvar qingeditor/config/show-transient-state-color-guide t
  "If non nil show the color guide hint for transient state keys.")

(defvar qingeditor/config/show-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters)")

(defvar qingeditor/config/smooth-scrolling t
  "If non nil smooth scrolling (native-scrolling) is enabled.
Smooth scrolling overrides the default behavior of Emacs which
recenters point when it reaches the top or bottom of the
screen.")

(defvar qingeditor/config/line-numbers nil
  "If non nil line numbers are turned on in all `prog-mode' and `text-mode'
derivatives. If set to `relative', also turns on relative line numbers.")

(defvar qingeditor/config/persistent-server nil
  "If non nil advises quit functions to keep server open when quitting.")

(defvar qingeditor/config/smart-closing-parenthesis nil
  "If non-nil pressing the closing parenthesis `)' key in insert mode passes
  over any automatically added closing parenthesis, bracket, quote, etc…
  This can be temporary disabled by pressing `C-q' before `)'. (default nil)")

(defvar qingeditor/config/smartparens-strict-mode nil
  "If non-nil smartparens-strict-mode will be enabled in programming modes.")

(defvar qingeditor/config/highlight-delimiters 'all
  "Select a scope to highlight delimiters. Possible values are `any',
`current', `all' or `nil'. Default is `all' (highlight any scope and
 emphasis the current one.")

(defvar qingeditor/config/whitespace-cleanup nil
  "delete whitespace while saving buffer. possible values are `all'
to aggressively delete empty lines and long sequences of whitespace, `trailing'
to delete only the whitespace at end of lines, `changed' to delete only
whitespace for changed lines or `nil' to disable cleanup.")

(defvar qingeditor/config/search-tools '("ag" "pt" "ack" "grep")
  "List of search tool executable names. `qingeditor' uses the first installed
tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.")

(defvar qingeditor/config/default-package-repository 'melpa-stable
  "The default package repository used if no explicit repository has been
specified with an installed package.
NOT USED FOR NOW :-)")

(defvar qingeditor/config/startup-lists '((recents . 5)
                                          (projects . 7))
  "Association list of items to show in the startup buffer of the form
`(list-type . list-size)`. If nil it is disabled.
Possible values for list-type are:
`recents' `bookmarks' `projects' `agenda' `todos'.
List sizes may be nil, in which case
`qingeditor/startup-buffer/startup-lists-length' takes effect.")

(defvar qingeditor/config/startup-buffer-responsive t
  "True if the home buffer should respond to resize events.")

(defvar qingeditor/config/excluded-packages '()
  "A list of packages that will not be install and loaded.")

(defvar qingeditor/config/frozen-packages '()
  "A list of packages that cannot be updated.")

(defvar qingeditor/config/verbose-loading nil
  "If non nil output loading progress in `*Messages*' buffer. (default nil)")

"figure out the target configuration full filename."
(let* ((env (getenv "QINGEDITOR_DIR"))
       (env-dir (when env (expand-file-name (concat env "/"))))
       (env-init-filename (and env-dir (expand-file-name "init.el" env-dir)))
       (no-env-dir-default
        (expand-file-name (concat qingeditor/user-home-dir ".qingeditor.d/")))
       (default-init-filename (expand-file-name ".qingeditor" qingeditor/user-home-dir))
       target-cfg-dir
       target-cfg-filename)
  (setq target-cfg-dir
        (cond
         ((and env (file-exists-p env-dir))
          env-dir)
         ((file-exists-p no-env-dir-default)
          no-env-dir-default)
         (t nil)))
  (let ((default-qingeditor-init-filename
          (when target-cfg-dir
            (concat target-cfg-dir "init.el"))))
    (setq target-cfg-filename
          (cond (env-init-filename)
                ((file-exists-p default-init-filename) default-init-filename)
                ((and target-cfg-dir (file-exists-p default-qingeditor-init-filename))
                 default-qingeditor-init-filename)
                (t default-init-filename))))
  (setq-default qingeditor/config/target-cfg-dir target-cfg-dir)
  (setq-default qingeditor/config/target-cfg-filename target-cfg-filename))

(provide 'qingeditor-config)
