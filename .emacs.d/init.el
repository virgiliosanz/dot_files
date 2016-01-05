;;; Package --- Summary

;;; Commentary:

;;; Code:
;; Turn on debugging (comment this out for normal use)
;;(setq debug-on-error t)
;;(setq debug-init t)

;; Global configuration
(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)
(setq make-backup-files nil)
;;(cua-mode t)
(auto-insert-mode t)
(transient-mark-mode t)
(delete-selection-mode t)
(auto-fill-mode t)
(setq-default fill-column 80)
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)
(setq global-font-lock-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(show-paren-mode t)
(visual-line-mode -1)
(setq-default show-trailing-whitespace t)
(setq next-line-add-newlines t)

;;; packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(setq package-list '(better-defaults
                     flycheck
                     ido smex
                     autopair
                     yasnippet
                     cmake-mode
                     auto-complete
                     magit
                     cc-mode auto-complete-clang
                     color-theme
                     cmake-ide rtags
))

;; fetch package list and install the missing ones
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Packages configuration

;; Theme
(require 'color-theme)
(color-theme-initialize)
(load-theme 'wombat)


;; flymake
(flymake-mode t)
(flycheck-mode t)

;; ido-mode
(require 'ido)
(ido-mode t)

;; parens
(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

;; auto-complete.org
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; ecb
;(require 'ecb)
;(ecb-activate)
;(require 'ecb-autoloads)
;(setq ecb-layout-name 'left8)
;                                        ;
;[C-c . g h] : Go to history
;[C-c . g m] : Go to methods
;[C-c . g s] : Go to sources:
;[C-c . g d] : Go to directories
;[C-c . g 1] : Main buffer

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; magit
(require 'magit)

;; CMake
(require 'cmake-mode)
(require 'rtags) ;; optional, must have rtags installed
(cmake-ide-setup)

;;; Languages

;; c-programming
(require 'cc-mode)
;(Setq c-default-style "k&r")
(setq c-default-style "linux")
(setq c++-default-style "stroustrup")
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(require 'auto-complete-clang) ;; auto-complete-clang-async for C/C++
(define-key c++-mode-map (kbd "C-S-<return>") 'ac-complete-clang)

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

;; Indent when enter is pressed
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;; Clean trailing white space
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Indent whole file
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;
(desktop-save-mode 1) ; save/restore opened files
