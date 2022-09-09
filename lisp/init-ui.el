;; -*- lexical-binding: t -*-

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
	:if (not nto/is-raspberry)
	:init
	(setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

	(load-theme 'modus-operandi)

	(set-face-attribute 'default nil
											:font "VictorMono"
											:weight 'light
											:height 130)

	(set-face-attribute 'fixed-pitch nil
											:font "VictorMono"
											:weight 'light
											:height 130)

	(set-face-attribute 'variable-pitch nil
											:font "VictorMono"
											:weight 'light
											:height 130))

;; I think this function could be written better
;; 	(if (>
;; 			 (string-to-number (nth 3 (split-string (current-time-string) " ")))
;; 			 17) ;; after 17:00
;; 			(load-theme 'modus-vivendi)
;; 		(load-theme 'modus-operandi)))

(use-package gruber-darker-theme)
(use-package emacs
	:if nto/is-raspberry
	:init
	(load-theme 'gruber-darker t))

(use-package emacs
  ;; :if (display-graphic-p)
  :init
	(setq initial-scratch-message nil)

	(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
	(setq mouse-wheel-progressive-speed nil)
	(setq mouse-wheel-follow-mouse 't)
	(setq scroll-step 1)


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
		(add-hook mode (lambda () (display-line-numbers-mode 1)))))

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

(use-package page-break-lines)

(use-package dashboard
	:init
	(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
	(setq dashboard-center-content t
				dashboard-show-shortcuts nil
				dashboard-set-navigator t
				dashboard-set-heading-icons t
				dashboard-set-file-icons t
				dashboard-set-footer nil
				dashboard-banner-logo-title nil)
	(if (not nto/is-raspberry)
			;; (setq dashboard-startup-banner 'logo)
			(setq dashboard-startup-banner 'official)
		(setq dashboard-startup-banner pi-banner))
	(setq dashboard-navigator-buttons
				`((;;Github
					 (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
						"Github"
						"My github"
						(lambda (&rest _) (browse-url "https://github.com/antonio-petrillo")))
					 (,(all-the-icons-fileicon  "emacs" :height 1.1 :v-adjust 0.0)
						"Emacs"
						"Vanilla Emacs configuration"
						(lambda (&rest _) (dired user-emacs-directory)))
					 (,(all-the-icons-faicon "linux" :height 1.1 :v-adjust 0.0)
						"Dots"
						"Dotfiles configuration for this machine"
						(lambda (&rest _) (dired dots-directory)))
					 )))
	(setq dashboard-items '((agenda . 5)))
	(dashboard-setup-startup-hook))

(use-package emojify
	:hook (after-init . global-emojify-mode)
	:init
	(nto/leader-keys
		"ie" 'emojify-insert-emoji
		"he" 'emojify-describe-emoji-at-point
		"hE" 'emojify-list-emojis))

(provide 'init-ui)
