(set-frame-font "Menlo 12")

;;(setq mac-option-key-is-meta t)
;;(setq mac-command-key-is-meta nil)
;;(setq mac-command-modifier 'meta)
;;(Setq mac-option-modifier nil)

;; https://stackoverflow.com/questions/6344389/osx-emacs-unbind-just-the-right-alt
;; To map the the left alt key with META and the right alt key with ALT, on the Mac OS X version of GNU Emacs, I use:
;;(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(use-package simpleclip
  :ensure t)
(simpleclip-mode 1)
