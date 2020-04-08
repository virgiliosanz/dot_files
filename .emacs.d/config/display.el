;; Disable GUI elements. Why?
;; .. they take up screen-space and are unnecessary, favor a minimal interface.
(tool-bar-mode -1)
;;(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable startup message. Why?
;; .. less noise is better.
(defun display-startup-echo-area-message () (message ""))

;; Visual bell. Why?
;; .. audible beeps are annoying.
(setq visible-bell 1)

;; Nice theme from Vim. Why?
;; .. personal preference.
;;(use-package inkpot-theme
;;  :demand t
;;  :config (load-theme 'inkpot t))
(use-package solarized-theme
  :demand t
  :config (load-theme 'solarized-wombat-dark t))

;; Show empty lines. Why?
;; .. without this you can't tell if there are blank lines at the end of the file.
(setq-default indicate-empty-lines t)

;; Keep cursors and highlights in current window only. Why?
;; .. it's not especially useful to show these in inactive windows.
(setq cursor-in-non-selected-windows 'hollow)
;; Highlight inactive windows. Why?
;; To keep the selection region when changing windows (when evil-mode is disabled).
(setq highlight-nonselected-windows t)
;; Disable bidirectional text support. Why?
;; .. slight performance improvement.
(setq bidi-display-reordering nil)

;; No startup screen. Why?
;; .. No need to distract us with unnecessary info.
(setq inhibit-startup-screen t)

;; Don't show buffer list on startup. Why?
;; .. buffer switching gets in the way, you can manually switch between them.
(setq inhibit-startup-buffer-menu t)

;; Hide mouse cursor while typing. Why?
;; .. it can overlap characters we want to see.
(setq make-pointer-invisible t)

;; Don't put two spaces after full-stop. Why?
;; .. one space after a full-stop is sufficient in most documentation & comments.
(setq sentence-end-double-space nil)

;; Window Title:
;; Include the buffer name & modified status. Why?
;; .. the buffer name helps to differentiate windows when selecting from a task list.
(setq-default frame-title-format "%b %& emacs")
;; Use diff-hl. Why?
;; .. shows lines you have modified from the last commit.
(use-package diff-hl
  :demand t
  :config (global-diff-hl-mode))

;; Highlights numbers. Why?
;; .. Emacs doesn't do this by default, use a package.
(use-package highlight-numbers
  :hook ((prog-mode) . highlight-numbers-mode))

;; Scale all text. Why?
;; .. it's useful sometimes to globally zoom in all text.
(use-package default-text-scale
  :demand t
  :init (setq default-text-scale-mode-map (make-sparse-keymap))
  :config (default-text-scale-mode))

;; ---------------
;; Display Options
;; ===============

;; Show line numbers. Why?
;; Helpful to give context when reading errors & the current line is made more prominent.
                                        ;(global-display-line-numbers-mode 1)

;; Show the column as well as the line. Why?
;; .. some compiler errors show the column which is useful to compare.
(setq column-number-mode t)

;; Show matching parentheses. Why?
;; .. handy for developers to match nested brackets.
(show-paren-mode 1)
;; Don't blink, it's too distracting.
(setq blink-matching-paren nil)
(setq show-paren-delay 0.2)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)

;; Disable word-wrap. Why?
;; .. confusing for reading structured text, where white-space can be significant.
(set-default 'truncate-lines t)

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

