;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;;
     ;; python
     ;; yaml
     ;; javascript
     ;; markdown
     ;; yaml
     ;; html
     ;; javascript
     ;; html
     ;; (python :variables
     ;;         python-sort-imports-on-save t
     ;;         python-enable-yapf-format-on-save t
     ;;         python-fill-column 99)
     ;; go
     ;; rust
     ;; latex
     ;;(c-c++ :variables
     ;;       c-c++-default-mode-for-headers 'c++-mode
     ;;       c-c++-enable-clang-format-on-save t
     ;;       c-c++-default-mode-for-headers 'c++-mode
     ;;       c-c++-clang-enable-support t)
     ;;ycmd

     emacs-lisp

     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     better-defaults

     ;;git
     ;;(version-control :variables
     ;;                 version-control-diff-tool 'diff-hl
     ;;                 version-control-global-margin t)

     ;;(spell-checking :variables
     ;;                spell-checking-enable-auto-dictionary t
     ;;                enable-flyspell-auto-completion t)

     org
     (mu4e :variables mu4e-installation-path " /usr/local/share/emacs/site-lisp/")

     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;;shell-scripts

     ;;syntax-checking
     ;;semantic

     (osx :variables osx-use-option-as-meta nil)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;;dotspacemacs-additional-packages '()
   dotspacemacs-additional-packages '(org-cliplink guess-language)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   ;;dotspacemacs-excluded-packages '()
   dotspacemacs-excluded-packages '(org-projectile)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; dotspacemacs-startup-lists '((recents . 5)
   ;;                              (projects . 2)
   ;;                              (agenda)
   ;;                              (todos))
   dotspacemacs-startup-lists '((recents . 8)
                                (projects . 5))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes
   '(
     spacemacs-dark
     wombat
     jbeans
     hc-zenburn
     spacemacs-light
     ir-black
     Leuven
     material
     odersky
     twilight
     zenburn
     )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Monaco"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'right-then-bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   ;; dotspacemacs-line-numbers 'relative
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   ;;dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

;; formatted-copy on OSX!
;; TODO: Generalize for unix & windows
(defun osx/formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout |pbcopy"))
      (kill-buffer buf))))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;;;;;;;; Generalities
  (setq user-full-name "Virgilio Sanz")
  (setq user-mail-address "virgilio.sanz@gmail.com")
  (setq server-socket-dir (expand-file-name "server" user-emacs-directory))

  ;; UTF8!
  (set-language-environment 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;; Remove trailing whitespace before saving a file
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

  ;; Force native indexing so .gitignore is respected
  (setq projectile-indexing-method 'native)

  ;; Editor Config
  ;;(editorconfig-mode 1)

  ;; IDO
  ;; (setq ido-everywhere t)
  ;; (setq ido-max-directory-size 100000)
  ;; (ido-mode (quote both))
  ;; ;; Use the current window when visiting files and buffers with ido
  ;; (setq ido-default-file-method 'selected-window)
  ;; (setq ido-default-buffer-method 'selected-window)

  ;;;;;;;;; C++ & ycmd
  ;; (add-hook 'c-mode-hook 'ycmd-mode)
  ;; (add-hook 'c++-mode-hook 'ycmd-mode)
  ;; (setq ycmd-server-command (list "python3" (file-truename "~/.emacs.d/ycmd/ycmd/")))
  ;; (setq ycmd-force-semantic-completion t)
  ;; (setq ycmd-startup-timeout 15)

  ;;(global-set-key [C-tab] 'clang-format-region)
  ;; (add-hook 'c++-mode-hook 'clang-format-bindings)
  ;; (defun clang-format-bindings ()
  ;;   (define-key c++-mode-map [tab] 'clang-format-buffer))
  ;; (add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))


  ;;;;;;;;; Org Mode - GTD
  (spacemacs/declare-prefix "o" "Org Mode")
  (spacemacs/set-leader-keys "oa" 'org-agenda)
  (spacemacs/set-leader-keys "ot" 'org-todo-list)
  ;;(spacemacs/set-leader-keys "ol" 'org-store-link)
  (spacemacs/set-leader-keys "oc" 'org-capture)
  (spacemacs/set-leader-keys "os" 'org-search-view)
  (spacemacs/set-leader-keys "om" 'org-mac-grab-link)
  (spacemacs/set-leader-keys "ok" 'osx/formatted-copy)
  (spacemacs/set-leader-keys "og" 'org-download-screenshot)
  (spacemacs/set-leader-keys "oi" 'org-mac-iCal)

  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-capture)
    (add-to-list 'org-modules 'org-habit)
    (add-to-list 'org-modules 'org-caldav)
    (add-to-list 'org-modules 'org-mac-iCal)
    (add-to-list 'org-modules 'org-protocol)
    (add-to-list 'org-modules 'org-cliplink)
    (add-to-list 'org-modules 'org-dashboard)
    (add-to-list 'org-modules 'org-mac-link)
    (add-to-list 'org-modules 'org-journal)
    (add-to-list 'org-modules 'org-agenda)
    (add-to-list 'org-modules 'org-pdfview)
    (add-to-list 'org-modules 'org-download)


    (defadvice org-capture
        (after make-full-window-frame activate)
      "Advise capture to be the only window when used as a popup"
      (if (equal "emacs-capture" (frame-parameter nil 'name))
          (delete-other-windows)))

    (defadvice org-capture-finalize
        (after delete-capture-frame activate)
      "Advise capture-finalize to close the frame"
      (if (equal "emacs-capture" (frame-parameter nil 'name))
          (delete-frame)))

    (setq org-agenda-files '("~/CloudStation/Org"))
;;    (setq org-agenda-text-search-extra-files '(agenda-archives
;;                                               "~/CloudStation/Org/inbox.org"))
    (setq org-directory "~/CloudStation/Org")
    (setq org-default-notes-file "~/CloudStation/Org/inbox.org")
    ;; (setq org-archive-location (concat "archive/archive-"
    ;;                                    (format-time-string "%Y" (current-time))
    ;;                                    ".org_archive::"))


    (setq org-highest-priority ?A)
    (setq org-lowest-priority ?C)
    (setq org-default-priority ?B)

    (setq org-todo-keywords '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)")
                              (sequence "WAITING(w!)" "|" "DONE(d!)")
                              (sequence "HOLD(h!)" "|" "DONE(d!)")
                              (sequence "|" "CANCELLED(c!)")
                              (sequence "|" "MEETING(m!)")
                              (sequence "PROJECT(p!)" "|" "DONE(d!)")
                              (sequence "OPPORTUNITY(o!)" "|" "DONE(d!)")))

    (setq org-todo-keyword-faces
          '(("TODO" . org-todo)
            ("NEXT" . org-todo)
            ("WAITING" . org-scheduled)
            ("MEETING" . org-scheduled-previously)
            ("DONE" . org-agenda-done)
            ("HOLD" . org-scheduled)
            ("CANCELLED" .  org-archived)
            ("PROJECT" .  org-target)
            ("OPPORTUNITY" .  org-target)))

    ;; Tags with fast selection keys
    (setq org-tag-alist (quote ((:startgroup)
                                ("@office" . ?o)
                                ("@home" . ?h)
                                ("@computer" . ?c)
                                ("@phone" . ?f)
                                ("@errand" . ?e)
                                (:endgroup)
                                ("PERSONAL" . ?p)
                                ("WORK" . ?w))))

    ;;(org-agenda-check-type t 'agenda)

    ;; Capture from browser
    ;; javascript:(function () {window.location.href='org-protocol://capture://l/'+encodeURIComponent(window.location.href)+'/'+encodeURIComponent(document.title)+'/'+encodeURIComponent(window.getSelection());})();
    ;; (setq org-capture-templates
    ;;       '(("l" "A link, for reading later." entry
    ;;          (file+headline "inbox.org" "From Browser")
    ;;          "* %:description\n%u\n\n%c\n\n%i"
    ;;          :empty-lines 1)))

    (setq org-capture-templates '(
                                  ("P" "Protocol" entry (file+headline , org-default-notes-file "From Browser")
                                   "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                  ("L" "Protocol Link" entry (file+headline , org-default-notes-file "From Browser")
                                   "* %? [[%:link][%:description]] \nCaptured On: %U")
                                  ))

    (setq org-refile-targets '((org-agenda-files :maxlevel . 5)))

    (setq org-log-done t)
    (setq org-enforce-todo-dependencies t)
    (setq org-agenda-dim-blocked-tasks t)
    ;;open agenda in current window
    (setq org-agenda-window-setup (quote current-window))
    ;;warn me of any deadlines in next 6 days
    (setq org-deadline-warning-days 6)
    ;;show me tasks scheduled or due in next fortnight
    ;;(setq org-agenda-span (quote fortnight))
    (setq org-agenda-span 5)

    ;;don't show tasks as scheduled if they are already shown as a deadline
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    ;;don't give awarning colour to tasks with impending deadlines
    ;;if they are scheduled to be done
    (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
    ;;don't show tasks that are scheduled or have deadlines in the
    ;;normal todo list
    ;(setq org-agenda-todo-ignore-scheduled (quote all))
    ;;sort tasks in order of when they are due and then by priority
    (setq org-agenda-sorting-strategy
          (quote
           ((agenda deadline-up priority-down)
            (todo priority-down category-keep)
            (tags priority-down category-keep)
            (search category-keep))))

    ;; Use full outline paths for refile targets - we file directly with IDO
    (setq org-refile-use-outline-path t)
    ;; Targets complete directly with IDO
    (setq org-outline-path-complete-in-steps nil)
    ;; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    ;; Use IDO for both buffer and file completion and ido-everywhere to t
    ;;(setq org-completion-use-ido t)
    ;; Use the current window for indirect buffer display
    (setq org-indirect-buffer-display 'current-window)

    ;; org-mac integration
    (setq org-agenda-include-diary t)

    ;; Org-agenda custom commands
    (setq org-agenda-custom-commands '(("o" "At the Office" tags-todo "@office")
                                      ("c" "At Computer" tags-todo "@computer")
                                      ("h" "At Home" tags-todo "@home")
                                      ("e" "Errands" tags-todo "@errand")
                                      ("p" "Phone Calls" tags-todo "@phone")
                                      ("w" "Work Related" tags-todo "WORK")
                                      ("r" "Personal Related" tags-todo "PERSONAL")
                                      ("d" "Agenda + to do"
                                       ((agenda) (todo "NEXT") (todo "WAITING") (todo "DONE")))))

    ;; Screenshots
    (if (equal system-type 'darwin)
        (setq org-download-screenshot-method "/usr/sbin/screencapture -i %s"))
    (setq-default org-download-image-dir "~/CloudStation/Org/ScreenShots")

    ;;
    ;; https://emacs.stackexchange.com/questions/14724/emacs-org-mode-how-to-make-agenda-views-of-blocked-parent-tasks
    ;;
    ;; (defun org-agenda-skip-if-not-blocked ()
    ;;   (let ((next-headline (save-excursion
    ;;                          (or (outline-next-heading) (point-max)))))
    ;;     (if (not (org-entry-blocked-p)) next-headline)))

    ;; (add-to-list 'org-agenda-custom-commands
    ;;              '("B" "Blocked entries" alltodo ""
    ;;                ((org-agenda-skip-function '(org-agenda-skip-if-not-blocked))
    ;;                 (org-enforce-todo-checkbox-dependencies nil))
    ;;                ))


    ;; (setq org-agenda-custom-commands
    ;;       '(("g" . "GTD contexts")
    ;;         ("go" "Office" tags-todo "@office")
    ;;         ("gc" "Computer" tags-todo "@computer")
    ;;         ("gh" "Home" tags-todo "@home")
    ;;         ("gp" "Phone" tags-todo "@phone")
    ;;         ("G" "GTD Block Agenda"
    ;;          ((todo "NEXT")
    ;;           (tags-todo "@office")
    ;;           (todo "TODO"))
    ;;          ((org-agenda-prefix-format "[ ] %T: ")
    ;;           (org-agenda-compact-blocks t)
    ;;           (ps-number-of-columns 2)
    ;;           (ps-landscape-mode t)))))

    ;;(org-reload)

    )

  ;;;;;;;;; Spell checking
  (setq spell-checking-enable-by-default t)
  (setq spell-checking-enable-auto-dictionary t)
  (setq enable-flyspell-auto-completion t)

  (setq guess-language-languages '(en es))
  (setq guess-language-min-paragraph-length 35)

  ;;;;;;;;; email - mu4e
  (setq mu4e-mu-binary "/usr/local/bin/mu")
  ;;; Set up some common mu4e variables
  (setq mu4e-maildir "~/Mail/work"
        mu4e-trash-folder "/Deleted Items"
        mu4e-refile-folder "/Archive"
        mu4e-sent-folder "/Sent Items"
        mu4e-drafts-folder "/Drafts"
        user-mail-address "vsanz@akamai.com"
        user-full-name "Virgilio Sanz"
        mu4e-get-mail-command "mbsync -a"
        mu4e-sent-messages-behavior 'delete
        mu4e-update-interval nil
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-show-images t
        mu4e-view-image-max-width 800
        ;;NEEDED FOR MBSYNC
        mu4e-change-filenames-when-moving t
        mu4e-view-show-addresses t)

  (setq mu4e-headers-fields
  '((:date          .  25)    ;; alternatively, use :human-date
    (:flags         .   6)
    (:from          .  22)
    (:subject       .  nil))) ;; alternatively, use :thread-subject


  (setq smtpmail-default-smtp-server "email.msg.corp.akamai.com"
        smtpmail-smtp-server "email.msg.corp.akamai.com"
        smtpmail-smtp-service 587)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

   ;;; Mail directory shortcuts
  (setq mu4e-maildir-shortcuts '(("/inbox" . ?g)))

  ;;; Bookmarks
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?i)))
  (setq mu4e-enable-mode-line t)


  ;;;;;;;;; System dependand
  ;;;;; OSX
  (when (eq system-type 'darwin)
    (setq mac-right-option-modifier 'none)
    (setq org-download-screenshot-method "/usr/sbin/screencapture -i %s"))

  ;;;;;;;;; Misc
  ;;(setq powerline-default-separator 'bar)
  ;; Wrap lines
  (spacemacs/toggle-truncate-lines-on)

  ;; Visual line navigation for textual modes
  (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

  ;; http://stackoverflow.com/questions/10171280/how-to-launch-gui-emacs-from-command-line-in-osx
  (x-focus-frame nil)
  ;; https://medium.com/@bobbypriambodo/blazingly-fast-spacemacs-with-persistent-server-92260f2118b7
  (evil-leader/set-key "q q" 'spacemacs/frame-killer))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (packed yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode company-anaconda anaconda-mode pythonic guess-language mu4e-maildirs-extension mu4e-alert ht unfill reveal-in-osx-finder pbcopy osx-trash osx-dictionary org-present org-pomodoro alert log4e gntp org-mime org-download org-cliplink mwim launchctl htmlize helm-company helm-c-yasnippet gnuplot fuzzy flyspell-popup flyspell-correct-helm flyspell-correct company-statistics company-quickhelp pos-tip company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "email.msg.corp.akamai.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
