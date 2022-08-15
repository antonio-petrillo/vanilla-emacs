(use-package evil-goggles
	:after evil
	:demand
	:init
	(setq evil-goggles-duration 0.05))

(use-package all-the-icons-dired
  :if (not nto/is-raspberry)
  :hook (dired-mode . (lambda () (interactive)
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode)))))

(use-package all-the-icons
	:if (not nto/is-raspberry)
  :demand)

(use-package all-the-icons-ibuffer
	:if (not nto/is-raspberry)
  :demand
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package all-the-icons-completion
	:if (not nto/is-raspberry)
  :after
	(marginalia all-the-icons)
  :hook
	(marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package doom-modeline
  :demand
  :init
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 20))

(use-package emacs
  ;; :if (display-graphic-p)
  :init
	(setq initial-scratch-message nil)

	(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
	(setq mouse-wheel-progressive-speed nil)
	(setq mouse-wheel-follow-mouse 't)
	(setq scroll-step 1)

	(setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

	(setq display-line-numbers-type 'relative)
	:config
	(dolist (mode '(;text-mode-hook
									prog-mode-hook
									conf-mode-hook))
		(add-hook mode (lambda () (display-line-numbers-mode 1))))

	(set-face-attribute 'default nil
											:font "VictorMono Nerd Font"
											:weight 'light
											:height 130)

	(set-face-attribute 'fixed-pitch nil
											:font "VictorMono Nerd Font"
											:weight 'light
											:height 130)

	(set-face-attribute 'variable-pitch nil
											:font "VictorMono Nerd Font"
											:weight 'light
											:height 130)

  (load-theme 'modus-operandi))

(use-package hide-mode-line
  :commands (hide-mode-line-mode))

(use-package rainbow-delimiters
	:hook
	(prog-mode . rainbow-delimiters-mode)
	(org-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
	:hook
	(prog-mode . rainbow-mode)
	(org-mode . rainbow-mode))

(use-package beacon
	:init
	(beacon-mode 1))

;; to add
;; + dashboard

;; (use-package dashboard
;; 	:demand
;; 	;; :init
;; 	;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;; 	:config
;; 	(dashboard-setup-startup-hook))

(use-package centered-cursor-mode
	:demand
	:config
	(global-centered-cursor-mode))

(provide 'init-ui)
