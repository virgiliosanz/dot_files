; https://medium.com/really-learn-programming/configuring-emacs-on-macos-a6c5a0a8b9fa
(setq os-is-mac nil)
(setq os-is-windows nil)
(setq os-is-linux nil)

(when (eq system-type 'darwin)
  (message "This is a mac system")

  (setq os-is-mac t)

  (set-frame-font "Monaco 12")
  (setq mac-command-key-is-meta nil)
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil)

  (use-package simpleclip
    :ensure t)
  (simpleclip-mode 1)

  (setq screenshot-cmd "/usr/sbin/screencapture -i -U %s")

  ;; TODO: test org-xclip and delete this
  (defun formated-copy ()
    "Export region to HTML, and copy it to the clipboard."
    (interactive)
    (save-window-excursion
      (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
             (html (with-current-buffer buf (buffer-string))))
        (with-current-buffer buf
          (shell-command-on-region
           (point-min)
           (point-max)
           "textutil -stdin -format html -convert rtf -stdout |pbcopy"))
        (kill-buffer buf))))
)

(when (eq system-type 'windows-nt)
  (message "This is a windows system")
  (setq os-is-windows t)
)

(when (eq system-type 'gnu/linux)
  (message "This is a linux system")
  (setq os-is-linux t)
)
