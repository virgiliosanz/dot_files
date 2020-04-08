;; ------------------
;; Operating system
;; ==================
(cond ((eq system-type 'windows-nt)
       ;; Windows-specific code goes here.
       )

      ((eq system-type 'gnu/linux)
       ;; Linux-specific code goes here.
       )

      ((eq system-type 'gnu/kfreebsd)
       ;; FreeBSD-specific code goes here.
       )

      ((eq system-type 'darwin)
       ;; OSX-specific code goes here.
       (setq mac-option-key-is-meta t)
       (setq mac-command-key-is-meta nil)
       (setq mac-command-modifier 'meta)
       (setq mac-option-modifier nil))

       ;; default Latin font (e.g. Consolas)
       (set-face-attribute 'default nil :family "Consolas")

       ;; default font size (point * 10)
       ;;
       ;; WARNING!  Depending on the default font,
       ;; if the size is not supported very well, the frame will be clipped
       ;; so that the beginning of the buffer may not be visible correctly.
       (set-face-attribute 'default nil :height 165)

       ;; use specific font for Korean charset.
       ;; if you want to use different font size for specific charset,
       ;; add :size POINT-SIZE in the font-spec.
       (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
      )

;; -------------------
;; Terminal vs Graphic
;; ===================
(cond ((display-graphic-p)
       ;; Graphical code goes here.
       )

      (t
       ;; Console-specific code
       ))
