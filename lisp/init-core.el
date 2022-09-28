;; -*- lexical-binding: t -*-

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
	(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))


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

	;; move to config.el (or early-init?)
	(setq user-full-name "Antonio Petrillo"
				user-mail-address "antonio.petrillo4@studenti.unina.it")

	(setq dots-directory "~/.dotfiles/")
	(setq resource-dir (expand-file-name "res/" user-emacs-directory))
	(setq pi-banner (expand-file-name "text_banner.txt" resource-dir))
	(setq xkcd-home (expand-file-name "xkcd/" resource-dir))

	(progn
		(if (not (file-directory-p resource-dir))
				(make-directory resource-dir))
		(if (not (file-directory-p xkcd-home))
				(make-directory xkcd-home)))

	(setq yasnippet-home (expand-file-name "yasnippet/" user-emacs-directory))
	(setq nto/is-raspberry (and (eq system-type 'gnu/linux)
															(string-match "aarch64" (shell-command-to-string "uname -m"))
															t)) ;; add something to check if emacs is running on my pi4
;;	(setq nto/is-mac nil) ;; I don't even have a mac right now, but I'm trying to find an M1 at a decent price
;;	(setq nto/is-ipad nil) ;; I can't install emacs on blink, I need to explore more iSh, but iSH is "only" an Alpine container, consider jaibreak
	;; (setq nto/is-gnu-linux t)
	;; (setq nto/is-winzozz nil)

	;; disable notification bell
	(setq ring-bell-function (quote ignore))
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

	(setq global-auto-revert-mode-non-file-buffers t)
	(global-auto-revert-mode 1)

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
			 (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
			 (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
			 (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
			 (define-key evil-insert-state-map (kbd "C-n") 'next-line)
			 (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
			 (define-key evil-insert-state-map (kbd "C-j") 'newline)
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
	:after evil
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
													:prefix ";"
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

	;; "c" '(:ignore t :which-key "code")
	;; to add:
	;; + eval lisp shortcut
	;; + lsp setup

	"f" '(:ignore t :which-key "file")
	"fD" '((lambda () (interactive) (delete-file (buffer-file-name))) :wk "delete")
	"ff" 'find-file
	"." 'find-file
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

	"i" '(:ignore t :wk "insert")
	"ic" 'insert-char

	"o" '(:ignore t :wk "open")

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
	:bind
	(:map corfu-map
				("C-j" . corfu-next)
				("C-k" . corfu-previous))
	(:map evil-insert-state-map ("C-k" . nil))
	:init
	(global-corfu-mode))

;; todo
;; + checkout dabbrev (it's built-in, the default is binded to M-/)

(use-package cape
  :bind (("C-c p" . completion-at-point) ;; capf
				 ("M-p" . completion-at-point) ;; capf
				 ;; ("C-c p p" . completion-at-point) ;; capf
         ;; ("C-c p t" . complete-tag)        ;; etags
         ;; ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ;; ("C-c p h" . cape-history)
         ;; ("C-c p f" . cape-file)
         ;; ("C-c p k" . cape-keyword)
         ;; ("C-c p s" . cape-symbol)
         ;; ("C-c p a" . cape-abbrev)
         ;; ("C-c p i" . cape-ispell)
         ;; ("C-c p l" . cape-line)
         ;; ("C-c p w" . cape-dict)
         ;; ("C-c p \\" . cape-tex)
         ;; ("C-c p _" . cape-tex)
         ;; ("C-c p ^" . cape-tex)
         ;; ("C-c p &" . cape-sgml)
         ;; ("C-c p r" . cape-rfc1345)
				 )
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package kind-icon
	:after corfu
	:demand
	:init
	(setq kind-icon-default-face 'corfu-default)
	(setq kind-icon-blend-background nil)
	(setq kind-icon-blend-frac 0.08)
	:config
	(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
	(with-eval-after-load 'modus-themes
		(add-hook 'modus-themes-after-load-theme-hook #'(lambda () (interactive) (kind-icon-reset-cache)))))

(use-package affe
	:init
	(nto/leader-keys
		"fa" '(:ignore t :which-key "affe")
		"faf" 'affe-find
		"fag" 'affe-grep))

(use-package yasnippet
	:init
	(setq yas-snippet-dirs '(yasnippet-home))
	(yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package perspective
	:demand t
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

(use-package hydra)

;; add ace window
(defhydra nto/hydra-window-management (:pre (message "window management")
                                       :color pink
                                       :hint nil
                                       :post (message "bye bye"))
  "
Movement^^      ^Split^         ^Switch^                ^Resize^                ^Action^
----------------------------------------------------------------------------------------
_h_ ← _H_ ← m   _v_ertical      _b_uffer                _=_  H ↑                _q_uit
_j_ ↓ _J_ ↓ m   _s_ horizontal  _f_ind files            _-_  H ↓                _t_erminal v
_k_ ↑ _K_ ↑ m   _d_ delete      _[_ next buffer         _._  W →                _T_erminal h
_l_ → _L_ → m   _c_ delete      _]_ prev buffer         _,_  W ←
^ ^             ^ ^             _C-k_ kill buffer       _e_ balance window
^ ^             ^ ^             _K_ kill this buffer    ^ ^
"

  ;; quit wm mode
  ("q" nil :color blue)
  ;; buffer & file
  ( "b" consult-buffer)
  ("[" previous-buffer)
  ("]" next-buffer)
  ("K" kill-this-buffer)
  ("C-k" kill-buffer)
  ("f" find-file)

  ;; window size
  ("=" evil-window-increase-height) ;; = so I don't need to use shift
  ("-" evil-window-decrease-height)
  ("." evil-window-increase-width)
  ("," evil-window-decrease-width)
  ("e" balance-windows)

  ;; split & swap
  ("c" evil-window-delete)
  ("d" evil-window-delete)
  ("S" ace-swap-window)
  ("v" evil-window-vsplit)
  ("s" evil-window-split)

	;; need improvment
	("t" vterm)
	("T" vterm)

  ;; movement
  ("h" evil-window-left)
  ("H" evil-window-move-far-left)
  ("j" evil-window-down)
  ("J" evil-window-move-very-bottom)
  ("k" evil-window-up)
  ("K" evil-window-move-very-top)
  ("l" evil-window-right)
  ("L" evil-window-move-far-left))

(nto/leader-keys
	"ow" '((lambda () (interactive) (nto/hydra-window-management/body)) :wk "window management"))

(general-define-key "M-o" 'nto/hydra-window-management/body)

(provide 'init-core)
