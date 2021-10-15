; https://medium.com/really-learn-programming/configuring-emacs-on-macos-a6c5a0a8b9fa
;(setq mac-command-modifier 'meta)
;(setq mac-option-modifier 'super)
;(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;(add-to-list 'default-frame-alist '(ns-appearance . light))
;(when (member "Fira Code" (font-family-list))
;  (add-to-list 'initial-frame-alist '(font . "Fira Code-14"))
;  (add-to-list 'default-frame-alist '(font . "Fira Code-14")))
;(set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
;(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)


(set-frame-font "Monaco 12")
(setq mac-command-key-is-meta nil)
(setq mac-option-key-is-meta t)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)

(use-package simpleclip
  :ensure t)
(simpleclip-mode 1)
