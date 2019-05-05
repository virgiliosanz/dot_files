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
(visual-line-mode -1)
(setq-default show-trailing-whitespace t)
(setq next-line-add-newlines t)
;;(winner-mode t)

;;; packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(setq package-list '(better-defaults
                     color-theme

                     flycheck
                     ido smex
                     autopair

                     magit

                     yasnippet

                     elpy

		     flymake
		     ycmd
		     company company-ycmd
		     flycheck flycheck-ycmd
		     eldoc
		     clang-format

                     evil

		     org
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

;; smex
(global-set-key (kbd "M-x") 'smex)

;; parens
(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; magit
(require 'magit)


;; evil mode
(require 'evil)
(evil-mode t)
(windmove-default-keybindings)


;;; Languages

;; c-programming
(require 'cc-mode)
;(setq c-default-style "k&r")
;(setq c++-default-style "stroustrup")
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(require 'clang-format)

(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)
; on-save: clang-format
(add-hook 'c++-mode-hook
    (lambda()
        (add-hook 'before-save-hook 'clang-format-buffer)))

;; python
(elpy-enable)

;; YouCompleteMe: ycmd
(setq ycmd-server-command
      (list "python2" "/Users/vsanz/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd"))

(add-hook 'ycmd-mode-hook 'company-ycmd-setup)
(add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)

(company-ycmd-setup)
(flycheck-ycmd-setup)
(when (not (display-graphic-p))
  (setq flycheck-indication-mode nil))
(require 'ycmd-next-error)
(require 'ycmd-eldoc)
(add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)

(global-company-mode)
(global-ycmd-mode)

;; org-mode


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

;;(desktop-save-mode 1) ; save/restore opened files

(provide 'init)
;;; init.el ends here
