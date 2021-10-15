;; Main Vim emulation package. Why?
;; .. without this, you won't have Vim key bindings or modes.
(use-package evil
  :demand t
  :config
  ;; Initialize.
  (evil-mode)

  ;; For some reasons evils own search isn't default.
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-case 'sensitive))

;; Use evil numbers to increment & decrement. Why?
;; .. evil-mode doesn't include this Vim functionality.
(use-package evil-numbers)
;; Vim increment/decrement keys.
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)


;; Perform actions on surrounding characters. Why?
;; .. while not part of Vim, it's a useful & common package for developers.
(use-package evil-surround
  :demand t
  :config
  ;; Initialize.
  (global-evil-surround-mode 1))


(use-package evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; Prompt for available keys after some delay. Why?
;; .. useful to see available keys after some delay, especially for evil-leader key.
(use-package which-key
  :demand t
  :config
  ;; Initialize.
  (which-key-mode))

(setq-default evil-shift-round nil)
(setq-default evil-indent-convert-tabs nil)

(defun my-indent-whole-buffer ()
  "Delete trailing whitespace, indent and untabify whole buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; --------------
;; Evil Mode Keys
;; ==============
;; Leader key support. Why?
;; .. this is part of Vim, but not part of evil-mode, use a package.
(use-package evil-leader
  :demand t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "g" 'find-grep)
  ;; Interactive current-file search.
  (evil-leader/set-key "s" 'swiper))


;;(which-key-declare-prefixes "f" "Files")
(evil-leader/set-key "ff" 'find-file)
(evil-leader/set-key "fi" 'ido-find-file)
(evil-leader/set-key "fg" 'find-grep)

;(which-key-declare-prefixes "b" "Buffer")
(evil-leader/set-key "bf" 'my-indent-whole-buffer)
(evil-leader/set-key "bb" 'counsel-switch-buffer)

;; Use secondary selection in insert mode, Why?
;; .. this is handy for quick middle mouse copy/paste while in insert mode.
(define-key evil-insert-state-map (kbd "<down-mouse-1>") 'mouse-drag-secondary)
(define-key evil-insert-state-map (kbd "<drag-mouse-1>") 'mouse-drag-secondary)
(define-key evil-insert-state-map (kbd "<mouse-1>") 'mouse-start-secondary)
;; De-select after copy, allows quick select-copy-paste.
(define-key evil-insert-state-map (kbd "<mouse-2>")
  (lambda (click)
    (interactive "*p")
    (when (overlay-start mouse-secondary-overlay)
      (mouse-yank-secondary click)
      (delete-overlay mouse-secondary-overlay))))


(use-package undo-tree
  :init
  (global-undo-tree-mode))
(evil-set-undo-system 'undo-tree)

