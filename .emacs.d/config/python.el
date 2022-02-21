(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package python
  :ensure t)

(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

(setq elpy-rpc-python-command "python3")

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(use-package pyvenv
  :ensure t
  :hook ((python-mode . pyvenv-mode))
  :config (global-pyenv-mode))


(use-package pyenv-mode
  :ensure t
  :config
    (defun projectile-pyenv-mode-set ()
      "Set pyenv version matching project name."
      (let ((project (projectile-project-name)))
        (if (member project (pyenv-mode-versions))
            (pyenv-mode-set project)
          (pyenv-mode-unset))))

    (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
    (add-hook 'python-mode-hook 'pyenv-mode))

(add-hook 'pyenv-mode-hook 'elpy-rpc-restart)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

(evil-leader/set-key-for-mode 'python-mode
 "bf" 'elpy-format-code
 "gd" 'elpy-goto-definition-other-window)
