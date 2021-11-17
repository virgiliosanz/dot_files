;; ------------------------------------------------------------------------------
;; Org-Mode
;; ==============================================================================
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib)

(use-package org-download
  :ensure t
  :config
  ;; add support to dired
  (add-hook 'dired-mode-hook 'org-download-enable))

;;(use-package org-bullets
;;    :hook (org-mode . org-bullets-mode))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.0))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :height 1.0))))
 '(org-level-9 ((t (:inherit outline-9 :height 1.0))))
 '(org-level-10 ((t (:inherit outline-10 :height 1.0))))
 '(org-level-11 ((t (:inherit outline-11 :height 1.0))))
 )

;; formatted-copy on OSX!
;; TODO: test org-xclip and delete this
(defun osx/formatted-copy ()
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

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-capture)
  ;; (add-to-list 'org-modules 'org-habit)
  ;; (add-to-list 'org-modules 'org-caldav)
  ;; (add-to-list 'org-modules 'org-mac-iCal)
  (add-to-list 'org-modules 'org-mac-link)
  (add-to-list 'org-modules 'org-protocol)
  (add-to-list 'org-modules 'org-cliplink)
  ;; (add-to-list 'org-modules 'org-pdfview)
  (add-to-list 'org-modules 'org-dashboard)
  (add-to-list 'org-modules 'org-journal)
  (add-to-list 'org-modules 'org-agenda)
  (add-to-list 'org-modules 'org-download)

  ;; Capture Images
  (if (equal system-type 'darwin)
      (setq org-download-screenshot-method "/usr/sbin/screencapture -i %s"))

  (setq-default org-download-image-dir "~/CloudStation/Org/ScreenShots")
  (setq org-startup-with-inline-images nil)
  (setq org-download-display-inline-images nil)

  ;; Open images outside of emacs
  (add-hook 'org-mode-hook
            '(lambda ()
               (setq org-file-apps (append '(("\\.png\\'" . default)) org-file-apps ))))

  (add-to-list 'org-file-apps '("\\.xls\\'" . default))

  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-frame)))

  (setq org-agenda-files '("~/CloudStation/Org"))
  (setq org-directory "~/CloudStation/Org")
  (setq org-default-notes-file "~/CloudStation/Org/inbox.org")
  ;;(setq org-archive-location "%s_archive::")
  ;;(setq org-archive-location "%s_archive::datetree/* Archived Tasks").
  (setq org-archive-location (concat "archive/"
                                     (format-time-string "%Y_" (current-time))
                                     "%s_archive::datetree/* Archived Tasks"))

  (setq org-highest-priority ?A)
  (setq org-default-priority ?B)
  (setq org-lowest-priority ?C)

  (setq org-todo-keywords '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)" "HOLD(h!)" "CANCELLED(c!)")
                            (sequence "TODO(t!)" "NEXT(n!)" "HOLD(h!)")
                            (sequence "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(c!)")))

  (setq org-todo-keyword-faces
        '(("TODO" . org-todo)
          ("NEXT" . org-todo)
          ("WAITING" . org-scheduled)
          ("DONE" . org-agenda-done)
          ("CANCELLED" .  org-archived)
          ("HOLD" . org-scheduled)))

  ;; Tags with fast selection keys
  (setq org-tag-alist (quote ((:startgroup)
                              ("@office" . ?o)
                              ("@home" . ?h)
                              ("@computer" . ?c)
                              ("@phone" . ?f)
                              ("@errand" . ?e)
                              (:endgroup)
                              (:startgroup)
                              ("Maybe" . ?y)
                              (:endgroup)
                              ("PROJECT" . ?j)
                              ("POC" . ?p)
                              ("CBR" . ?b)
                              ("OPPORTUNITY" . ?r)
                              ("MEETING" . ?m))))

  (setq org-use-tag-inheritance nil)

  ;; Replace org-set-tags with org-set-tags-command in keybinding
  (evil-leader/set-key-for-mode 'org-mode ":" 'org-set-tags-command)

  ;; Capture from browser
  ;; javascript:(function () {window.location.href='org-protocol://capture://l/'+encodeURIComponent(window.location.href)+'/'+encodeURIComponent(document.title)+'/'+encodeURIComponent(window.getSelection());})();
  ;; (setq org-capture-templates
  ;;       '(("l" "A link, for reading later." entry
  ;;          (file+headline "inbox.org" "From Browser")
  ;;          "* %:description\n%u\n\n%c\n\n%i"
  ;;          :empty-lines 1)))

  (setq org-capture-templates '(("P" "Protocol" entry (file+headline , org-default-notes-file "From Browser")
                                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                ("L" "Protocol Link" entry (file+headline , org-default-notes-file "From Browser"))
                                ("T" "Todo [inbox]" entry
                                 (file+headline org-defaults-notes-file "Tasks")
                                 "* TODO %i%?"   "* %? [[%:link][%:description]] \nCaptured On: %U")
                                ("C" "Tickler/Calendar" entry
                                 (file+headline "~/Sync/orgs/gtd/tickler.org" "Tickler")
                                 "* %i%? \n %U")))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 5)))

  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'note)
  (setq org-log-redeadline t)
  (setq org-log-done 'time)

  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-dim-blocked-tasks t)
  ;;open agenda in current window
  (setq org-agenda-window-setup (quote current-window))
  ;;warn me of any deadlines in next 6 days
  (setq org-deadline-warning-days 7)
  ;;(setq org-agenda-span (quote fortnight))
  ;;(setq org-agenda-span 5)
  (setq org-agenda-span 4
        org-agenda-start-on-weekday 1
        org-agenda-start-day "-1d")
  ;; (setq org-agenda-span 7
  ;;      org-agenda-start-on-weekday 1
  ;;      )

  ;;don't show tasks as scheduled if they are already shown as a deadline
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)

  ;;don't give a warning colour to tasks with impending deadlines
  ;;if they are scheduled to be done
  (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
  ;;(setq org-agenda-skip-scheduled-if-done t)
  ;;(setq org-agenda-skip-deadline-if-done t)

  ;;don't show tasks that are scheduled or have deadlines in the
  ;;normal todo list
  (setq org-agenda-todo-ignore-scheduled (quote all))
  ;;sort tasks in order of when they are due and then by priority
  (setq org-agenda-sorting-strategy
        (quote
         ((agenda deadline-up priority-down)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep))))

  ;; Use full outline paths for refile targets - we file directly with IDO
  (setq org-refile-use-outline-path t)
  ;; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  ;; Use IDO for both buffer and file completion and ido-everywhere to t
  ;;(setq org-completion-use-ido t)
  ;; Use the current window for indirect buffer display
  (setq org-indirect-buffer-display 'current-window)

;;  (setq org-agenda-include-diary t)

  (setq org-hide-leading-stars t)

  ;; Org-agenda custom commands
  (setq org-agenda-custom-commands '(("o" "At the Office" tags-todo "@office")
                                     ("c" "At Computer" tags-todo "@computer")
                                     ("h" "At Home" tags-todo "@home")
                                     ("e" "Errands" tags-todo "@errand")
                                     ("p" "Phone Calls" tags-todo "@phone")
                                     ("i" "Import diary from iCal" agenda ""
                                      ((org-agenda-mode-hook
                                        (lambda () (org-mac-iCal)))))
                                     ("r" "Weekly Review"
                                       ((tags-todo "POC")
                                        (tags-todo "CBR")
                                        (tags-todo "OPPORTUNITY")
                                        (tags-todo "PROJECT")
                                        (tags-todo "Maybe")
                                        (todo "TODO")
                                        (todo "WAITING")
                                        (todo "HOLD")
                                        (todo "DONE")
                                        (todo "CANCELLED")))
                                     ("d" "Agenda + to do"
                                      ((agenda)
                                       (tags-todo "POC")
                                       (tags-todo "CBR")
                                       (tags-todo "OPPORTUNITY")
                                       (tags-todo "PROJECT")))
                                     ("w" "Working"
                                      ((tags-todo "@computer")
                                       (tags-todo "POC")
                                       (tags-todo "CBR")
                                       (tags-todo "OPPORTUNITY")
                                       (agenda)
                                       (tags-todo "@errand")
                                       (tags-todo "@phone")))))

  ;; New line at 80th mode
  (add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; Show overview when open
  (setq org-startup-folded t)


  ;;(org-reload)
  )

;; evil-org
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Configure leader key
(evil-leader/set-key-for-mode 'org-mode
  "t" 'org-todo
  "T" 'org-show-todo-tree
  "v" 'org-mark-element
  "a" 'org-agenda
  "c" 'org-archive-subtree
  "l" 'evil-org-open-links
  "C" 'org-resolve-clocks)

;; Org Mode - evil
;;(which-key-declare-prefixes "o" "Org Mode")
(evil-leader/set-key "oa" 'org-agenda)
(evil-leader/set-key "ot" 'org-todo-list)
(evil-leader/set-key "om" 'org-mac-grab-link)
;;(evil-leader/set-key "oc" 'org-capture)
(evil-leader/set-key "os" 'org-search-view)
(evil-leader/set-key "ok" 'osx/formatted-copy)
(evil-leader/set-key "og" 'org-download-screenshot)
;;(evil-leader/set-key "oi" 'org-download-clipboard)
(evil-leader/set-key "of" 'org-fill-paragraph)
(evil-leader/set-key "ol" 'org-insert-link)
(evil-leader/set-key "od" 'org-toggle-link-display)

(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)


;; Global keys for capturing
;;(global-set-key (kbd "C-c l") 'org-store-link)
;;(global-set-key (kbd "C-c c") 'org-capture)
;;(global-set-key (kbd "C-c a") 'org-agenda)

;; Calendar from O365
;; npm i -g icsorg
