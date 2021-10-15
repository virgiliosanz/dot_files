(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "config/defaults.el")
(load-user-file "config/evil.el")
(load-user-file "config/completion.el")
(load-user-file "config/display.el")
(load-user-file "config/calendar.el")
(load-user-file "config/orgmode.el")

;;;;; ----  Programming
(load-user-file "config/programming.el")
;;(load-user-file "config/python.el")
;;(load-user-file "config/c++.el")
;;(load-user-file "config/rust.el")
;;(load-user-file "config/asm.el")
(load-user-file "config/latex.el")

;;;;; ----  OS
(load-user-file "config/osx.el")
;;(load-user-file "config/mswindows.el")
;;(load-user-file "config/linux.el")

(message "*** Emacs loaded in %s with %d garbage collections."
     (format "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time))) gcs-done)

