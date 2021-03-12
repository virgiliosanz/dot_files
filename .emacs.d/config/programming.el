;; --------------------------------------------------------------------
;; Defaults for programming
;; ========================

(semantic-mode 1)

;; On-the-fly syntax checking
(use-package flycheck
  :ensure t
  :defer 2
  :diminish
  flycheck-mode
  :init
  (global-flycheck-mode t)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/CloudStation/Dev/" "~/CloudStation/Tests/" "~/Documents/Code/"))
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))

;; SMARTPARENS
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))

;; Indent after parents - http://xenodium.com/emacs-smartparens-auto-indent/index.html
(defun indent-between-pair (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))

;; highlight FIXME, TODO and the like
(use-package fic-mode
  :ensure t
  :diminish fic-mode
  :commands turn-on-fic-mode
  :config (add-hook 'prog-mode-hook 'turn-on-fic-mode))

;; Highlight matching delimiters
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :diminish rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; -----
;; Shell
;; -----
(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local fill-column 120)
            (setq-local tab-width 4)
            (setq-local evil-shift-width 4)
            (setq-local indent-tabs-mode nil)
            (setq-local ffip-patterns '("*.sh"))))

;; -----------------------
;; Yasnippet
;; =======================
(use-package yasnippet                  ; Snippets
  :ensure t
  :config
  (setq yas-snippet-dirs '(yasnippet-snippets-dir))
  (setq yas-verbosity 1)                      ; No need to be so verbose
  (setq yas-wrap-around-region t))
;;
(use-package yasnippet-snippets         ; Collection of snippets
  :ensure t)
;;
(yas-global-mode)
(yas-reload-all)

;; iedit
(use-package iedit
 :ensure t)
(evil-leader/set-key "ie" 'iedit-mode)


;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))
