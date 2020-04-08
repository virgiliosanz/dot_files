;; --------------------
;; Assembly
;; ====================
;; https://vishnudevtj.github.io/notes/assembly-in-emacs
(use-package x86-lookup
  :ensure t
  :config
  (setq  x86-lookup-pdf "~/CloudStation/Dev/64-iA32-Instruction-set-reference-vol2.pdf")
  )

(global-set-key (kbd "C-h x") #'x86-lookup)
;(evil-leader/set-key-for-mode
;  'c++-mode "hx" 'x86-lookup)

(use-package nasm-mode
  :ensure t
  :config
  (add-hook 'asm-mode-hook 'nasm-mode)
  )
