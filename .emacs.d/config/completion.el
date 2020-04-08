;; Ivy completion. Why?
;; .. makes compleating various prompts for input much more friendly & interactive.
(use-package ivy
  :demand t
  :config
  (ivy-mode)

  ;; Always show half the window height. Why?
  ;; Useful when searching through large lists of content.
  (setq ivy-height-alist `((t . ,(lambda (_caller) (/ (frame-height) 2)))))
  (setq ivy-display-style 'fancy)

  ;; Vim style keys in ivy (holding Ctrl).
  (define-key ivy-minibuffer-map (kbd "C-j") 'next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'previous-line)

  (define-key ivy-minibuffer-map (kbd "C-h") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-done)

  ;; open and next
  (define-key ivy-minibuffer-map (kbd "C-M-j") 'ivy-next-line-and-call)
  (define-key ivy-minibuffer-map (kbd "C-M-k") 'ivy-previous-line-and-call)

  (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-done)

  ;; so we can switch away
  (define-key ivy-minibuffer-map (kbd "C-w") 'evil-window-map))

;; Use for auto-complete. Why?
;; .. saves typing, allows multiple back-ends based on the current language/mode.
(use-package company
  :ensure t

  :commands
  (company-complete-common
   company-dabbrev)

  :init
  (global-company-mode)

  :config
  (setq company-idle-delay 0)
  (setq company-minimun-prefix-lenght 2)

  ;; Increase maximum number of items to show in auto-completion. Why?
  ;; Seeing more at once gives you a better overview of your options.
  (setq company-tooltip-limit 40)

  ;; Don't make abbreviations lowercase or ignore case. Why?
  ;; Many languages are case sensitive, so changing case isn't helpful.
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)

  ;; set default `company-backends'
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf)          ; completion-at-point-functions
          (company-abbrev company-dabbrev)
          ))

  ;; Keymap: hold Ctrl for Vim motion. Why?
  ;; .. we're already holding Ctrl, allow navigation at the same time.
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-l") 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") 'company-abort)
  (define-key company-active-map (kbd "<C-return>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package company-statistics
  :ensure t
  :init
  (company-statistics-mode))

(use-package company-web
  :ensure t)

(use-package company-try-hard
  :ensure t
  :bind
  (("C-<tab>" . company-try-hard)
   :map company-active-map
   ("C-<tab>" . company-try-hard)))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

;; ---------------------
;; Evil Keys for company
;; =====================
;; Auto complete using words from the buffer.
(define-key evil-insert-state-map (kbd "C-n") 'company-dabbrev)
;; Comprehensive auto-complete.
(define-key evil-insert-state-map (kbd "C-SPC") 'company-complete-common)

;; Use swiper for interactive buffer search. Why?
;; .. quickly search the buffer if useful.
(use-package swiper
  :commands (swiper)
  :config

  ;; Go to the start of the match instead of the end. Why?
  ;; .. allows us to operate on the term just jumped to (look up reference for e.g.)
  (setq swiper-goto-start-of-match t))

;; Use counsel for project wide searches. Why?
;; .. interactive project wide search is incredibly useful.
(use-package counsel
  :commands (counsel-git-grep counsel-switch-buffer))

;; Find file in project. Why?
;; .. interactively narrowing down other files in the project is very useful.
(use-package find-file-in-project
  :commands (find-file-in-project))

;; ---------------------------
;; IDO - Interactive Do Things
;; ===========================

;; ido-mode enhances emacs switch buffer command and opening file command.
;; It AUTOMATICALLY show list of choices as you type (no need to press Tab first).
;;
;; Alt+x ido-mode to toggle it on/off.
;;
;; Alt+x ido-switch-buffer [Ctrl+x b] to switch buffer.
;;
;; Alt+x ido-find-file [Ctrl+x Ctrl+] to open file.

(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (ido-mode 1))

;; -----------------------
;; SMEX
;; =======================
(use-package smex
  :config
  (smex-initialize))

(define-key evil-motion-state-map ";" 'smex)
(define-key evil-motion-state-map ":" 'evil-ex)


;; -----------------------
;; LSP
;; =======================
(use-package lsp-mode
  :commands lsp
  :config (require 'lsp-clients))

(use-package lsp-ui)

;; -----------------------
;; Yasnipper
;; =======================
;; (use-package yasnippet                  ; Snippets
;;   :ensure t
;;   :config
;;   (setq yas-snippet-dirs '(yasnippet-snippets-dir))
;;   (setq yas-verbosity 1)                      ; No need to be so verbose
;;   (setq yas-wrap-around-region t))
;;
;; (use-package yasnippet-snippets         ; Collection of snippets
;;   :ensure t)
;;
;; (yas-global-mode)
;; (yas-reload-all)
