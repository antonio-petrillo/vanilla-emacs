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

(provide 'init-prog)
