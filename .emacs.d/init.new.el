;;; Package --- Summary

;;; Commentary:
;; Emacs as a C/C++ Editor/IDE (Part I): auto-complete, yasnippet, and auto-complete-c-headers
;; http://youtu.be/HTUE03LnaXA
;; Emacs as a C/C++ Editor/IDE (Part 2): iedit, flymake-google-cpplint, google-c-style
;; http://youtu.be/r_HW0EB67eY
;; Emacs as a C/C++ Editor/IDE (Part 3): cedet mode for true intellisense
;; http://youtu.be/Ib914gNr0ys

;;; Code:
;; Turn on debugging (comment this out for normal use)
;(setq debug-on-error t)
;(setq debug-init t)

;; Global configuration
(setq user-full-name "Virgilio Sanz")
(setq user-mail-address "virgilio.sanz@gmail.com")

(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)
(setq make-backup-files nil)
;(cua-mode t)
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
(global-visual-line-mode t)
(setq-default word-wrap t)

(setq-default show-trailing-whitespace t)
(setq next-line-add-newlines t)

(setq package-list '(better-defaults
                     autopair
                     color-theme
		     auto-complete
		     yasnippet
		     auto-complete-c-headers
		     iedit
		     ido
		     flymake-google-cpplint
		     flymake-cursor
		     column-marker
		     google-c-style))

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; fetch package list and install the missing ones
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Packages configuration
(require 'color-theme)
(color-theme-initialize)
(load-theme 'wombat)

(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

(require 'ido)
(ido-mode t)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(require 'yasnippet)
(yas-global-mode 1)
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; Para añadir directorios de include a my:ac-c-header-init
  ;; Para saber que hay que poner: gcc -xc++ -E -c -
  ;;  (add-to-list 'achead:include-directorios '"PATH")
  ;;  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/7.0.2/include/")
  )
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;; Fix iedit bug in Mac
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; In order for google-cpplint to work we need to install cpplint.py
;; sudo pip install cpplint
(defun my:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/usr/local/bin/cpplint"))
  (flymake-google-cpplint-load))

(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; A ver que tal se lleva esto con el google style
(require 'cc-mode)
(setq c-default-style "linux")
(setq c++-default-style "stroustrup")

(semantic-mode 1)
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

(global-ede-mode 1)
(ede-cpp-root-project "my project" :file "~/tmp/emacs.c++/my_program/src/main.cpp"
		      :include-path '("/../my_inc"))

(global-semantic-idle-scheduler-mode 1)

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

;; save/restore opened files
(desktop-save-mode 1)

;; UTF-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
	(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
