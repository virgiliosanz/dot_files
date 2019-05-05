;;; init.el --- Atila Neves's custom emacs config

;; Copyright (C) 2013

;; Author:  <aalvesne@cisco.com>
;; Keywords: init

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

;;

;;; Code:

(set 'gc-cons-threshold 100000000)

;; old init-packages
(when (< emacs-major-version 24)
  (error "Only works with Emacs versions 24 and newer")
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default)))
 '(electric-indent-mode +1)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(inferior-lisp-program "clisp")
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(org-agenda-files
   (quote
    ("~/org/work.org" "~/org/work.html" "~/org/personal.org")) t)
 '(safe-local-variable-values
   (quote
    ((cmake-ide-dir . "/build/sla/sla32")
     (cmake-ide-dir . "/build/gravastar")
     (cmake-ide-dir . "/build/sla/sla")
     (cmake-ide-compile-command . "ninja -C /build/sla/sla && /build/sla/sla/unittests/ut && /build/sla/sla/unittests/nxos/ut_nxos"))))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)


(require 'package)
(require 'cl-lib)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(unless package-activated-list (package-refresh-contents)) ;get list of packages
(let ((mypackages '(ac-dcd ace-jump-mode ace-window ag
                           aggressive-indent auto-complete-clang
                           auto-complete-c-headers change-inner
                           cmake-ide cmake-mode
                           color-theme-solarized color-theme
                           column-enforce-mode company d-mode
                           elisp-slime-nav elpy auto-complete epc
                           ctable concurrent deferred
                           expand-region find-file-in-project
                           flycheck flycheck-cask
                           flycheck-haskell flycheck-dmd-dub
                           feature-mode fuzzy go-mode
                           haskell-mode highlight-indentation
                           htmlize idomenu key-chord
                           markdown-mode multiple-cursors
                           ninja-mode nose pkg-info dash popup
                           rsense rtags smart-mode-line
                           smartparens smartscan smex
                           solarized-theme switch-window
                           virtualenvwrapper
                           whole-line-or-region xcscope yaml-mode
                           yasnippet jedi)))
  (when (or (null package-activated-list) (cl-set-difference package-activated-list mypackages))
    (mapc
     (lambda (package) (or (package-installed-p package)
                           (package-install package)))
     mypackages)))

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (let ((default-directory "~/.emacs.d/elpa/"))
    (normal-top-level-add-subdirs-to-load-path)))


;; import functions from funcs.el
(require 'funcs)

;; old init-appearance
(require 'appearance-funcs)
(init-appearance)

;; old init-misc-options
; Trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
; Disable for c-mode (t means local) for now to not introduce merge conflicts
(add-hook 'c-mode-hook (lambda()
                         (make-local-variable 'before-save-hook)
                         (remove-hook 'before-save-hook 'delete-trailing-whitespace)
                         (setq show-trailing-whitespace 1)))
(add-hook 'c-mode-hook #'column-enforce-mode)

; indentation
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

; backups
(setq make-backup-files nil) ; Don't want any backup files

; parentheses
(show-paren-mode t)

; enable commands that are disabled for dummies
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

; show columns
(setq column-number-mode t)


; No more typing the word 'yes' out
(defalias 'yes-or-no-p 'y-or-n-p)


;; old init-c-common

;C/C++ indentation
(c-add-style "msi-style"
             '((c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . ((substatement-open before after)))
               (c-offsets-alist . ((topmost-intro        . 0)
                                   (topmost-intro-cont   . 0)
                                   (substatement         . 4)
                                   (substatement-open    . 0)
                                   (statement-case-open  . 4)
                                   (statement-cont       . 4)
                                   (access-label         . -4)
                                   (inclass              . 4)
                                   (inline-open          . 4)
                                   (innamespace          . -4)
                                   ))))

;C/C++ indentation
(c-add-style "atila-style"
             '((c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . ((substatement-open before after)))
               (c-offsets-alist . ((topmost-intro        . 0)
                                   (topmost-intro-cont   . 0)
                                   (substatement         . 4)
                                   (substatement-open    . 0)
                                   (statement-case-open  . 4)
                                   (statement-cont       . 4)
                                   (access-label         . -4)
                                   (inclass              . 4)
                                   (inline-open          . 4)
                                   ))))

; Fix lambdas in C++11 and D
(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (or (equal major-mode 'c++-mode) (equal major-mode 'd-mode))
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                     ;;   and with unclosed brace.
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ; no additional indent
          ad-do-it)))                   ; default behavior

;; c/c++ mode style
(add-hook 'c-mode-common-hook
          '(lambda()
             (c-set-style "msi-style")
             ))

(add-hook 'c++-mode-hook
          (lambda()
            (setq c-macro-cppflags "-x c++ -std=c++1y")))

;; my own font-lock settings for C++11
(require 'atila-c++)
(atila-c++11-font-lock)

;; auto-insert include guards in header files
;; autoinsert C/C++ header
(define-auto-insert
    (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C/C++ header")
    '(nil
      (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
                 (nopath (file-name-nondirectory noext))
                 (ident (concat (upcase nopath) "_H_")))
        (concat "#ifndef " ident "\n"
                        "#define " ident "\n\n\n"
                        "\n\n#endif // " ident "\n"))
      ))

(add-hook 'find-file-hook 'auto-insert)

; Don't ask to reload TAGS if newer, just do it
(setq tags-revert-without-query 1)

;; old init-modes
; D mode
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

; Go mode
(autoload 'go-mode "go-mode" "Major mode for editing Go code." t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

; CMake mode
(autoload 'cmake-mode "cmake-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

; sh-mode for zsh files
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

; ruby-mode for Vagrantfiles
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))

; markdown-mode
(autoload 'mardown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; haskell-mode
(require 'haskell-interactive-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
(add-hook 'haskell-mode-hook (lambda ()
                               (electric-indent-mode -1)))
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook (lambda () (global-set-key (kbd "<f5>") 'haskell-process-cabal-build)))


; support for cask projects
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

;; old init-buffer-file

;; old init-default-packages
; whitespace
(require 'whitespace)
(setq whitespace-style '(tabs tab-mark)) ;white space mode only for tabs
(global-whitespace-mode 1)

; tramp
(autoload 'tramp "tramp")
(eval-after-load "tramp" '(setq tramp-default-method "scp"))

; ido
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

; uniquify: better names for identically named buffers
(require 'uniquify)

;; old init-custom-packages
;yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; autocomplete
(require 'company)
(setq company-backends (delete 'company-semantic company-backends))
(setq company-idle-delay 0)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-quick-help-delay 0.4)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map [return] nil)
(define-key ac-complete-mode-map "\r" nil)
(define-key ac-mode-map [C-return] 'auto-complete)
(setq ac-candidate-limit 100) ;; do not stall with too many results

;; I think the function below is magical, I removed
;; it and auto-complete stopped working in "weird" buffers"
(defun auto-complete-mode-maybe ()
  "Auto complete everywhere."
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))

(require 'auto-complete-clang)
(require 'c++-include-files)

;; rtags
(require 'rtags)
(require 'rtags-ac)
(require 'company-rtags)

(rtags-enable-standard-keybindings c-mode-base-map)
(setq rtags-completions-enabled t)

;; cmake-ide
(cmake-ide-setup)
(setq cmake-ide-flags-c++ (append '("-std=c++1y")
                                 (mapcar (lambda (path) (concat "-I" path)) (c++-include-paths))))
(setq cmake-ide-flags-c '("-I/usr/include"))


; autocomplete headers
(add-hook 'c++-mode-hook (lambda()
                           (require 'auto-complete-c-headers)
                           (setq ac-sources '(ac-source-c-headers ac-source-clang ac-source-yasnippet))
                           (setq company-backends '(company-rtags company-clang company-dabbrev-code company-keywords company-yasnippet company-files company-dabbrev))
                           (add-to-list 'ac-sources 'ac-source-c-headers)
                           (setq achead:include-directories
                                 (append achead:include-directories (c++-include-paths)))
                           (auto-complete-mode 0)
                           (company-mode 1)
                           (global-set-key [C-return] 'company-complete-common)))

(add-hook 'c-mode-hook (lambda()
                         (require 'auto-complete-c-headers)
                         (setq ac-sources '(ac-source-c-headers ac-source-clang ac-source-yasnippet))
                         (setq company-backends '(company-rtags company-clang company-dabbrev-code company-keywords company-yasnippet company-files company-dabbrev))

                         (add-to-list 'ac-sources 'ac-source-c-headers)
                         (add-to-list 'achead:include-directories "/usr/include")
                         (auto-complete-mode 0)
                         (company-mode 1)
                         (global-set-key [C-return] 'company-complete-common)))

(defun toggle-ac ()
  "Toggle between auto-complete and company."
  (interactive)
  (if (bound-and-true-p auto-complete-mode)
      (progn
        (auto-complete-mode 0)
        (company-mode 1)
        (global-set-key [C-return] 'company-complete-common))
    (progn
      (company-mode 0)
      (auto-complete-mode 1)
      (global-set-key [C-return] 'auto-complete))))

                                        ; autocomplete ruby with rsense
(setq rsense-home "/opt/rsense-0.3")
(require 'rsense)
(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

;scope
(require 'xcscope)
(cscope-setup)
(setq cscope-index-recursively t)
(setq cscope-use-inverted-index t)
(define-key cscope-minor-mode-keymap (kbd "C-c s d") 'cscope-find-global-definition-no-prompting)

(require 'virtualenvwrapper)

(let ((venv-path "~/coding/python/virtualenv"))
  (when (file-exists-p venv-path)
    (setq venv-location venv-path)
    (setq python-environment-directory venv-location)

    ;; To avoid the annoying issue of not being able to edit Python unless in a virtualenv,
    ;; the below lines make sure we're always in one. To make jedi work properly, we
    ;; use virtualenv-workon even when launched from a shell already in a virtualenv
    (unless (getenv "VIRTUAL_ENV")
      (venv-workon "p3"))
    (setq python-shell-virtualenv-path (getenv "VIRTUAL_ENV"))

    (require 'pymacs)
    (setq py-load-pymacs-p nil)
    (pymacs-load "ropemacs" "rope-")

    ;; jedi for Python
    (setq jedi:complete-on-dot t)
    (add-hook 'python-mode-hook 'jedi:setup)
    ))

;; disable electric-indent in Python mode
(add-hook 'python-mode-hook (lambda () (electric-indent-mode -1)))

;; disable any other source of autocompletion
;; before this it was (ac-source-jedi-direct ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)
(add-hook 'python-mode-hook (lambda() (setq ac-sources '(ac-source-jedi-direct))))

;flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'd-mode-hook 'flycheck-dmd-dub-set-variables)
(add-hook 'c++-mode-hook (lambda() (setq flycheck-clang-language-standard "c++1y")))

; ffip
(autoload 'find-file-in-project "find-file-in-project")
(setq ffip-project-file '(".git" ".hg" ".svn"))
(setq ffip-patterns
      '("*.html" "*.org" "*.txt" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl"
        "*.sh" "*.erl" "*.hs" "*.ml"
        "*.h" "*.c" "*.hpp" "*.cpp" "*.hxx" "*.cxx" "*.d" "*.feature"))
(setq ffip-limit 32000)

; htmlize                               ; ;
(autoload 'htmlize "htmlize")
(setq htmlize-output-type "inline-css")

;;; Seen on Emacs Rocks
; Expand region by semantic units       ; ;
(autoload 'expand-region "expand-region")

(autoload 'multiple-cursors "multiple-cursors")
(autoload 'ace-jump-mode "ace-jump-mode")
(autoload 'ace-window "ace-window")
(autoload 'camelcase-word-or-region "change-case")
(autoload 'snakecase-word-or-region "change-case")

;smex for easier M-x commands           ; ;
(require 'smex)

; smart mode line                       ; ;
(sml/setup)
;(sml/apply-theme 'automatic)           ; ;

; key-chord                             ; ;
(require 'key-chord)
(key-chord-mode 1)

; kill whole line or region             ; ;
(whole-line-or-region-mode 1)

;; old init-user
;; Conclude init by setting up specifics for the current user
(let ((user-settings-dir (concat user-emacs-directory "users/" user-login-name)))
  (when (file-exists-p user-settings-dir)
    (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$"))))

;; org
(setq org-log-done t)
(setq org-agenda-files '("~/org/work.org" "~/org/personal.org"))
(add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))


;; find C/C++ headers in other directories
(setq ff-search-directories '("." "../include" "../src" "../internal_inc"))

;; DCD
(add-to-list 'ac-modes 'd-mode)
(require 'ac-dcd)
(add-hook 'd-mode-hook 'ac-dcd-setup)
(add-hook 'd-mode-hook #'(lambda () (setq ac-sources '(ac-source-dcd ac-source-yasnippet))))


;; smartscan
(global-smartscan-mode)

;; jump to definition in elisp
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; check parens after saving elisp buffers
(add-hook 'after-save-hook (lambda () (when (equal major-mode 'emacs-lisp-mode)
                                        (check-parens))))
;; no message in scratch buffer
(setq initial-scratch-message nil)

;; aggressively indent
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;; actually turn on smart parens
(smartparens-global-mode t)
(require 'smartparens-config)

; smart parentheses
(show-smartparens-mode 1)


;; reuse frames
(setq-default display-buffer-reuse-frames t)

;; compilation mode scrolls nicely
(setq compilation-scroll-output 'first-error)

;; coloured compilation buffer
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; keybindings

(global-set-key [home] 'back-to-indentation-or-beginning)
(global-set-key [kp-home] 'back-to-indentation-or-beginning)
(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(define-key global-map (kbd "C-c C-e") 'eval-and-replace)
(global-set-key (kbd "C-x n i") 'narrow-to-region-indirect)
(global-set-key (kbd "C-c g") 'google)
(global-set-key (kbd "C-c y") 'youtube)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "C-M-o") 'open-previous-line)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-unset-key (kbd "C-z")) ; pesky ctrl-Z minimisation
(global-set-key (kbd "M-o") 'ace-window)
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "<C-tab>") 'ff-find-other-file)))

(global-set-key (kbd "C-S-r") 'ido-find-file-in-tag-files)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-O") 'change-outer)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-x o") 'switch-window)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; keychord bindings
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)
(key-chord-define-global "jl" 'goto-line-with-feedback)
(key-chord-define-global "JJ" 'ido-switch-buffer)
(key-chord-define-global "xx" 'smex)

;; cmake-ide
(global-set-key (kbd "<f5>") 'cmake-ide-compile)

;; smart-parens
(define-key smartparens-mode-map (kbd "<C-S-left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "<C-S-right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-S-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-S-<right>") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-<backspace>") 'sp-backward-unwrap-sexp)

(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

(define-key smartparens-mode-map (kbd "H-t") 'sp-prefix-tag-object)
(define-key smartparens-mode-map (kbd "H-p") 'sp-prefix-pair-object)
(define-key smartparens-mode-map (kbd "H-s c") 'sp-convolute-sexp)
(define-key smartparens-mode-map (kbd "H-s a") 'sp-absorb-sexp)
(define-key smartparens-mode-map (kbd "H-s e") 'sp-emit-sexp)
(define-key smartparens-mode-map (kbd "H-s p") 'sp-add-to-previous-sexp)
(define-key smartparens-mode-map (kbd "H-s n") 'sp-add-to-next-sexp)
(define-key smartparens-mode-map (kbd "H-s j") 'sp-join-sexp)
(define-key smartparens-mode-map (kbd "H-s s") 'sp-split-sexp)

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "<f5>") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

(define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
(define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "M-.") 'rope-goto-definition)
            (define-key python-mode-map (kbd "M-,") 'rope-find-occurrences)
            ))
(add-hook 'd-mode-hook
          (lambda ()
            (define-key d-mode-map (kbd "M-.") 'ac-dcd-goto-definition)
            (define-key d-mode-map (kbd "M-,") 'ac-dcd-search-symbol)
            ))

(provide 'init.el)
;;; init.el ends here
