;; ----------------
;; C & C++ Language
;; ----------------
(use-package ycmd
  :ensure t
  :init
  (add-hook 'c++-mode-hook #'ycmd-mode)
  (add-hook 'c-mode-hook #'ycmd-mode)
  :config
  (set-variable 'ycmd-server-command '("/usr/local/bin/python3" "~/.vim/plugged/YouCompleteMe/third_party/ycmd/ycmd"))
  (set-variable 'ycmd-global-config (expand-file-name "~/.ycm_extra_conf.py")))

(use-package company-ycmd
  :ensure t
  :init
  (company-ycmd-setup)
  :config
  ;;(add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd)))
  (add-to-list 'company-backends 'company-ycmd))

(use-package flycheck-ycmd
  :ensure t)

;; Show argument list in echo area
(use-package eldoc
  :diminish eldoc-mode
  :init (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))

(evil-leader/set-key-for-mode
  'c++-mode "bf" 'clang-format-buffer)
