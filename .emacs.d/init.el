;;; Package --- Summary

;;; Commentary:
(message "Loading my Emacs init file...")

;;; Code:
;; Turn on debugging (comment this out for normal use)
(setq debug-on-error t)
; (setq debug-init t)

;; Global configuration
(defalias 'yes-or-no-p 'y-or-n-p)
(setq version-control t
      delete-old-versions t
      backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
      kept-new-versions 3
      kept-old-versions 1)
(cua-mode t)
(auto-insert-mode t)
(transient-mark-mode t)
(delete-selection-mode t)
(setq-default fill-column 80)
(column-number-mode t)
(setq next-line-add-newlines nil)
(setq global-font-lock-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(show-paren-mode t)
(visual-line-mode -1)

;;; Packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Packages to be used (and installed if not)
(setq package-list '(better-defaults 
                     flyspell 
                     flymake flymake-cursor
                     ido smex
                     autopair
                     yasnippet
                     auto-complete
                     slime
                     web-mode js2-mode
                     eshell
                     magit
                     org
                     cc-mode
                     ecb))

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Package configuration
;; ido-mode
(ido-mode t)

;; parens
(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

;; yasnippet
(require 'yasnippet)
(yas-global-mode)

;; auto-complete.org
(require 'auto-complete-config)
(ac-config-default)

;; SLIME
(require 'slime-autoloads)
(setq inferior-lisp-program "/opt/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))
;; To make SLIME connect to your lisp whenever a lisp file is opened
(add-hook 'slime-mode-hook
          (lambda ()
            (unless (slime-connected-p)
              (save-excursion (slime)))))

;; web-mode
(require 'web-mode)
(setq web-mode-indent-style 2)

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; eshell
(require 'eshell)

;; magit
(require 'magit)

;; org-mode
(require 'org)
(require 'remember)
;; A leer algún día
;; http://members.optusnet.com.au/~charles57/GTD/gtd_workflow.html
;; http://members.optusnet.com.au/~charles57/GTD/orgmode.html

;; Theme
(load-theme 'wombat)

;; c-programming
(require 'cc-mode)
(setq c-default-style "k&r")
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; Emacs code browser
(require 'ecb)
(require 'ecb-autoloads)
(setq ecb-layout-name "left1")
(setq ecb-compile-window-height 8)
; (ecb-auto-activate)
;; Default keys
;; 
;;    Jump to the directory window        CTRL-c . gd
;;    Jump to the history window          CTRL-c . gh
;;    Jump to the last window you were in CTRL-c . gl
;;    Jump to the first editor window     CTRL-c . g1
;;    Jump to the methods window          CTRL-c . gm

;;; Keyboard
;; Move between buffers: C-x <right> y C-x <left>
;; M-r cycle top, bottom and middle of current screen.
;; M-g M-g to go line.
;; ESC < go to the beginning of the file 
;; ESC > go to the end of the file 
(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
(global-set-key (kbd "C-j") 'top-join-line)


;; Indent Whole Buffer: http://emacsblog.org/2007/01/17/indent-whole-buffer/
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; Initialize server
(load "server")
(unless (server-running-p)
  (server-start))

(message "Emacs ends...")
(provide 'init)
;;; init.el ends here
