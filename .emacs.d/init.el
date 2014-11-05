;;; Package --- Summary

;;; Commentary:

;;; Code:
;; Turn on debugging (comment this out for normal use)
(setq debug-on-error t)
(setq debug-init t)

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
;;                     flyspell
                     flymake flymake-cursor
                     tramp
;;                     guru-mode
                     ido smex
                     autopair
                     yasnippet
                     auto-complete
                     web-mode js2-mode
                     magit
                     go-mode go-autocomplete go-errcheck go-snippets flymake-go
                     cc-mode auto-complete-clang
                     elpy
                     org org-plus-contrib))

;; fetch package list and install the missing ones
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Packages configuration
;; Theme
(load-theme 'wombat)

;;; Tools
;; Guru-mode -> Learnnnn!!!
;;(require 'guru-mode)
;;(guru-global-mode +1)

;; ido-mode
(ido-mode t)

;; TRAMP (Transport Remote Access, Multi Protocol)
(require 'tramp)
(setq tramp-default-method "ssh")

;; parens
(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

;; auto-complete.org
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; magit
(require 'magit)

;;; Languages

;; Python
;; https://github.com/jorgenschaefer/elpy/wiki/Keybindings
;; https://github.com/jorgenschaefer/elpy/wiki/Usage
(when (require 'elpy nil t)
  (elpy-enable)
  (elpy-clean-modeline))
;;(define-key ac-completing-map (kbd "<up>") nil)
;;(define-key ac-completing-map (kbd "<down>") nil)
;;(define-key ac-completing-map (kbd "RET") nil)
;;(define-key ac-completing-map (kbd "<return>") nil)
(setq elpy-rpc-backend "jedi")
;;(setq elpy-rpc-backend "rope")

;; web-mode
(require 'web-mode)
(setq web-mode-indent-style 2)

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; c-programming
(require 'cc-mode)
;(setq c-default-style "k&r")
(setq c-default-style "stroustrup")
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; auto-complete-clang-async for C/C++
(require 'auto-complete-clang)

(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
;; (ac-set-trigger-key "TAB")
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
(define-key ac-mode-map  [(control return)] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)


;; Haskell
;; http://tim.dysinger.net/posts/2014-02-18-haskell-with-emacs.html
;;(require 'haskell-mode)
;;(require 'scion)

;; Go
(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)
(require 'go-autocomplete)
;;(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

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

;; Key modifiers for Mac OS X Emacs.app in spanish MBP keyboard
(global-set-key (kbd "M-1") "|")
(global-set-key (kbd "M-2") "@")
(global-set-key (kbd "M-3") "#")
(global-set-key (kbd "M-º") "\\")
(global-set-key (kbd "M-ç") "}")
(global-set-key (kbd "M-+") "]")

(provide 'init)
;;; init.el ends here
