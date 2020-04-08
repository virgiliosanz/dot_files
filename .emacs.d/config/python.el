(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package pyvenv
  :ensure t
  :hook ((python-mode . pyvenv-mode)))

(evil-leader/set-key-for-mode
  'python-mode "bf" 'elpy-format-code)
