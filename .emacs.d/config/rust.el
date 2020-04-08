(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp))
(setq rust-format-on-save t)


;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
