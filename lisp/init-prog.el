;; -*- lexical-binding: t -*-
(use-package projectile
	:demand t
	:diminish projectile-mode
	:config (projectile-mode)
	:init
	(nto/leader-keys
		"p" '(:ignore t :wk "projectile")
		"pp" 'projectile-command-map
		"pd" 'projectile-dired
		"pf" 'projectile-find-file
		"ps" 'projectile-swith-project
		"pF" 'projectile-ripgrep
		"pc" 'projectile-compile-project
		"pt" 'projectile-run-vterm))


(use-package lsp-mode
	:commands
	(lsp lsp-deferred)
	:hook
	(lsp-mode . lsp-enable-which-key-integration)
	:init
	(setq lsp-keymap-prefix "C-c l")
	(add-hook 'lsp-mode-hook #'corfu-lsp-setup))

(use-package lsp-ui
	:commands lsp-ui-mode)

(use-package go-mode)

(use-package python-mode
	:hook (python-mode . lsp-deferred)
	:custom
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
	(dap-python-debugger 'debugpy)
	:config
	(require 'dap-python))

(use-package pyenv
	:config
	(pyenv-mode 1))

(use-package clojure-mode)
(use-package cider) ;; remember cider jack in

(provide 'init-prog)
