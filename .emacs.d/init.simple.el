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
;;(visual-line-mode -1)
(setq-default show-trailing-whitespace t)
(setq next-line-add-newlines t)

;;; packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(setq package-list '(color-theme
                     ido
                     smex
                     autopair
                     iedit
                     auto-complete
                     auto-complete-clang-async
                     auto-complete-c-headers
                     rtags
                     cmake-ide
                     magit
                     elpy
                     evil))

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

;; ido-mode
(require 'ido)
(ido-mode t)
(require 'smex)

;; parens
(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

;; iedit
(require 'iedit)
(define-key global-map (kbd "C-c ;") 'iedit-mod) ;; Fix iedit bug in Mac

;; auto-complete.org
(require 'auto-complete)
(require 'auto-complete-clang-async)
(require 'auto-complete-c-headers)
(require 'auto-complete-config)
(ac-config-default)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)

;; magit
(require 'magit)
(global-set-key (kbd "<f7>") 'magit-status)

;; timemachine
(require 'git-timemachine)

;; cc-mode
(require 'cc-mode)
(defun my/c-mode ()
  "Default style for C."
  (c-set-style 'stroustrup))
;  (c-set-style 'linux))
(add-hook 'c-mode-hook 'my/c-mode)

(defun my/c++-mode ()
  "Default style for C++."
  (c-set-style 'stroustrup))
(add-hook 'c++-mode-hook 'my/c++-mode)

;; python
(elpy-enable)

;; vim keys
(require 'evil)
(evil-mode t)

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
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; save/restore opened files
(desktop-save-mode 1)

(provide 'init)
;;; init.el ends here
