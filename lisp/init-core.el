;; setup straight.el
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-recipes-gnu-elpa-use-mirror t)
(setq straight-check-for-modification nil)
(setq use-package-always-defer t)

;; bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; set straight to use 'use-package'
(straight-use-package 'use-package)
(setq comp-deferred-compilation-black-list nil)
(setq use-package-compure-statistics t)

(use-package gcmh
	     :demand
	     :config
	     (gcmh-mode 1))

(use-package helpful
	     :after evil
	     :init
	     (setq evil-lookup-func #'helpful-at-point)
	     :bind
	     ([remap describe-function] . helpful-callable)
	     ([remap describe-command]  . helpful-command)
	     ([remap describe-variable] . helpful-variable)
	     ([remap describe-key]      . helpful-key))


(use-package no-littering
	:demand
	:init
	(setq custom-file (expand-file-name "custom.el" (concat user-emacs-directory "lisp")))
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))


;; undo system (see evil setup in init-keybidings.el)
(use-package undo-fu)

(use-package undo-fu-session
	     :after undo-fu
	     :demand
	     :init
	     (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
	     :config
	     (global-undo-fu-session-mode))

(use-package emacs
  :init
	;; follow symlinks
	(setq vc-follow-symlinks t)
	;; don't warn for large files
	(setq large-file-warning-threshold nil)
	;; delete highighted text
	(delete-selection-mode t)
	;; don't create backup file
	(setq make-backup-files nil
				auto-save-default nil
				create-lockfiles  nil)
	;; less noise when compiling elisp
	(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
	(setq native-comp-async-report-warnings-errors nil)
	(setq load-prefer-newer t)

	(recentf-mode t)
	(setq recentf-exclude '(,(expand-file-name "straight/build/" user-emacs-directory)
													,(expand-file-name "eln-cache/" user-emacs-directory)
													,(expand-file-name "etc/" user-emacs-directory)
													,(expand-file-name "var/" user-emacs-directory)))

	;; clean up the mode line
	(display-time-mode -1)
	(setq column-number-mode t)

	;; use common convention for indentation by default
	(setq-default indent-tabs-mode t)
	(setq-default tab-width 2)

  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

  ;; for my mental sanity
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package evil
	     :demand
	     :init
	     (setq evil-want-integration t)
	     (setq evil-want-keybinding nil)
	     (setq evil-want-C-i-jump t)
	     (setq evil-want-Y-yank-to-eol t)
			 (setq evil-undo-system 'undo-fu)
	     (setq evil-search-module 'evil-search)
	     (setq evil-split-window-below t)
	     (setq evil-vsplit-window-below t)
	     (setq evil-auto-indent nil)
	     (evil-mode 1)
	     :config
	     (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
			 (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
			 (evil-global-set-key 'motion "j" 'evil-next-visual-line)
			 (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
	     (define-key evil-motion-state-map "_" 'evil-end-of-line)
	     (define-key evil-motion-state-map "0" 'evil-beginning-of-line)
	     (evil-set-initial-state 'messages-buffer-mode 'normal)
	     (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
	     :after evil
	     :demand
	     :config
	     (evil-collection-init))

(use-package evil-snipe
	     :after evil
	     :demand
	     :config
			 (setq evil-snipe-spillover-scope 'visible)
	     (evil-snipe-mode +1)
	     (evil-snipe-override-mode +1))

(use-package evil-surround
	:after evil
	:demand
	:config
	(global-evil-surround-mode 1))

(use-package evil-indent-plus
	     :after evil
	     :demand
	     :config
	     (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
	     (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
	     (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
	     (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
	     (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
	     (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))

(use-package evil-escape 
	     :after evil
	     :demand
	     :config
			 (evil-escape-mode t)
	     (setq-default evil-escape-key-sequence "jk")
	     (setq-default evil-escape-delay 0.2)
	     (setq evil-escape-unordered-key-sequence t))

;; checkout:
;; + evil-mc (hlissner)
;; + evil-iedit (sylbrn)

(use-package consult)

(use-package general
  :demand
  :config
	(general-evil-setup)
	(general-create-definer nto/leader-keys
													:states '(normal insert visual emacs)
													:keymaps 'override
													:prefix "SPC"
													:global-prefix "C-SPC")
	
	(general-create-definer nto/local-leader-keys
													:states '(normal visual insert visual emacs)
													:keymaps 'override
													:prefix ","
													:global-prefix "C-;"))

(nto/leader-keys
	"SPC" '(execute-extended-command :which-key "execute command")
	"<escape>" 'keyboard-escape-quit
	"b" '(:ignore t :which-key "buffer")
	"br" 'revert-buffer
	"bd" 'kill-current-buffer
	"bi" 'ibuffer
	"bb" 'consult-buffer
	;; to add:
	;; + jump to scratch

	"c" '(:ignore t :which-key "code")
	;; to add:
	;; + eval lisp shortcut
	;; + lsp setup

	"f" '(:ignore t :which-key "file")
	"fD" '((lambda () (interactive) (delete-file (buffer-file-name))) :wk "delete")
	"ff" 'find-file
	"." 'find-file
	"fa" '(:ignore t :which-key "affe")
	"faf" 'affe-find
	"fag" 'affe-grep
	"fs" 'save-buffer
	;; to add:
	;; + rename file
	;; + copy file (something cp)

	"h" '(:ignore t :which-key "help")
	"he" 'view-echo-area-messages
	"hf" 'describe-function
	"hF" 'describe-face
	"hl" 'view-lossage
	"hL" 'find-library
	"hm" 'describe-mode
	"hk" 'describe-key
	"hK" 'describe-keymap
	"hp" 'describe-package
	"hv" 'describe-variable

	"t" '(:ignore t :which-key "toggle")
	"tl" '(display-line-numbers-mode :wk "line numbers")
	"tt" '((lambda () (interactive) (modus-themes-toggle)) :wk "toggle theme")

	"u" '(universal-argument :wk "universal")

	"w" '(:ignore t :which-key "window")
	"ww" 'ace-window
	"wh" 'evil-window-left
	"wj" 'evil-window-down
	"wk" 'evil-window-up
	"wl" 'evil-window-right
	"wm" '(delete-other-windows :wk "maximize")
	"wd" 'delete-window
	"wc" 'delete-window
	"wD" 'kill-buffer-and-window
	"wr" 'winner-redo
	"wu" 'winner-undo
	"w=" 'balance-window
	"w," 'evil-window-decrease-width
	"w." 'evil-window-increase-width
	"wv" 'evil-window-vsplit
	"ws" 'evil-window-split
	"wc" 'evil-window-delete
	"w+" 'evil-window-increase-height
	"w-" 'evil-window-decrease-height)

(use-package winner
	:after evil
	:config
	(winner-mode))

(use-package which-key
  :demand
  :init
  (nto/leader-keys
    "?" 'which-key-show-top-level)
  :config
  (setq which-key-idle-delay 0.05)
  (which-key-mode))

(use-package ace-window
	:custom
	(aw-scope 'frame)
	(aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy
	:init
	(nto/leader-keys
		"j" '(:ignore t :which-key "jump")
		"jj" '(avy-goto-char :which-key "jump to char")
		"jw" '(avy-goto-word-0 :which-key "jump to word")
		"jl" '(avy-goto-line :which-key "jump to line")))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; move to config.el
(setq user-full-name "Antonio Petrillo"
			user-mail-address "antonio.petrillo4@studenti.unina.it")

(use-package vertico
	:demand
	:bind
	(:map vertico-map
				("C-j" . vertico-next)
				("C-k" . vertico-previous)
				("C-f" . vertico-exit)
				("<escape>" . minibuffer-keyboard-quit)
				("M-<backspace>" . vertico-directory-delete-word))
	:init
	(setq vertico-count 20)
	(setq vertico-cycle t)
	(vertico-mode))

(use-package marginalia
	:after vertico
	:custom
	(marginalia-annotators '(marginalia-annotators-heavy marginalia-anootators-light nil))
	:init
	(marginalia-mode))

(use-package orderless
	:init
	(setq completion-styles '(orderless)
				completion-category-defaults nil
				completion-category-overrides '((file (styles partial-completion)))))

(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))

(use-package sudo-edit
  :commands (sudo-edit))

(use-package corfu
	:custom
	(corfu-cycle t)
	(corfu-auto t)
	(corfu-separator ?\s)
	(corfu-scroll-margin 5)
	:hook
	((prog-mode . corfu-mode)
	 (eshell-mode . corfu-mode)
	 (org-mode . corfu-mode))
	:init
	(global-corfu-mode))
;; todo
;; + add cape by minad
;; + checkout dabbrev

(use-package affe)

(use-package yasnippet
	:config
	(setq yas-snippet-dirs '(concat user-emacs-directory "yasnippet"))
	(yas-global-mode 1))

(use-package perspective
	:init
	(nto/leader-keys
		"TAB" '(:ignore t :which-key "perspective")
		"TAB TAB" 'persp-switch
		"TAB `" 'persp-switch-last
		"TAB d" 'persp-kill
		"TAB h" 'persp-prev
		"TAB p" 'persp-prev
		"TAB l" 'persp-next
		"TAB n" 'persp-next)
	:init
	(setq persp-state-default-file (expand-file-name ".persp" user-emacs-directory))
	(setq persp-suppress-no-prefix-key-warning t)
	:config
	(persp-mode)
	(add-hook 'kill-emacs-hook #'persp-state-save))

(use-package magit
	:init
	(nto/leader-keys
	 "g"   '(:ignore t :which-key "git")
	 "gs"  'magit-status
	 "gd"  'magit-diff-unstaged
	 "gc"  'magit-branch-or-checkout
	 "gl"   '(:ignore t :which-key "log")
	 "glc" 'magit-log-current
	 "glf" 'magit-log-buffer-file
	 "gb"  'magit-branch
	 "gP"  'magit-push-current
	 "gp"  'magit-pull-branch
	 "gf"  'magit-fetch
	 "gF"  'magit-fetch-all
	 "gr"  'magit-rebase))

(provide 'init-core)
