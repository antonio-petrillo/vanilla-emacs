;; -*- lexical-binding: t -*-

(use-package dired
	:straight (:type built-in)
	:init
  ;; dired integration
  (evil-define-key 'normal dired-mode-map
 	 (kbd "h") 'dired-up-directory
 	 (kbd "l") 'dired-find-file
 	 (kbd "m") 'dired-mark
 	 (kbd "t") 'dired-toggle-marks
 	 (kbd "u") 'dired-unmark
 	 (kbd "C") 'dired-do-copy
 	 (kbd "D") 'dired-do-delete
 	 (kbd "J") 'dired-goto-file
 	 (kbd "M") 'dired-do-chmod
 	 (kbd "O") 'dired-do-chown
 	 (kbd "R") 'dired-do-rename
 	 (kbd "T") 'dired-do-touch
 	 (kbd "Y") 'dired-copy-filename-as-kill ; copies filename to kill ring.
 	 (kbd "+") 'dired-create-directory
 	 (kbd "-") 'dired-up-directory
 	 (kbd "% l") 'dired-downcase
 	 (kbd "% u") 'dired-upcase
 	 (kbd "; d") 'epa-dired-do-decrypt
 	 (kbd "; e") 'epa-dired-do-encrypt)

	(setq wdired-allow-to-change-permissions t)
	(setq dired-hide-details-hide-symlink-targets t)
	(setq dired-recursive-copies 'always)
	(setq dired-recursive-deletes 'top)
	(setq dired-omit-files "^\\.[^.]\\|$Rhistory\\|$RData\\|__pycache__")
	(setq dired-listing-switches "-lah")
  (setq ls-lisp-dirs-first t)
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq ls-lisp-use-insert-directory-program nil)
	(setf dired-kill-when-opening-new-dired-buffer t))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; dependencies
;; libvterm cmake
(use-package vterm
	:if (not nto/is-raspberry)
  :init
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
	;; (nto/leader-keys
	;; 	"oT" 'vterm
	;; 	"v" '(:ignore t :wk "vterm")
	;; 	"vo" 'vterm)
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(use-package multi-vterm
	:config
	(add-hook 'vterm-mode-hook
			(lambda ()
			(setq-local evil-insert-state-cursor 'box)
			(evil-insert-state)))
	(define-key vterm-mode-map [return]                      #'vterm-send-return)

	(setq vterm-keymap-exceptions nil)
	(evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
	(evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
	(evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
	(evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
	(evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(use-package vterm-toggle
	:init
	(nto/leader-keys
		"ot" 'vterm-toggle-cd
		"oT" 'vterm-toggle
		"v" '(:ignore t :wk "vterm")
		"vo" 'vterm-toggle-cd
		"vn" 'vterm-toggle-forward
		"vj" 'vterm-toggle-forward
		"vp" 'vterm-toggle-backward
		"vk" 'vterm-toggle-backward))

;; dependencies
;; pacman -S fd poppler ffmpegthumbnailer mediainfo imagemagick tar unzip
;;
;; (use-package dirvish
;; 	:if (not nto/is-raspberry)
;; 	:init
;; 	(nto/leader-keys
;; 		;; "." 'dirvish-side
;; 		;; "fj" 'dirvish)
;; 		"fd" '(:ignore t :which-key "dirvish")
;; 		"fds" 'dirvish-side
;; 		"fd." 'dirvish)
;; 	;; (dirvish-override-dired-mode)
;; 	(setq dirvish-peek-mode t)
;; 	:custom
;; 	(dirvish-attributes '(file-size collapse subtree-state vc-state git-msg)))

(use-package pdf-tools
	:if (not nto/is-raspberry)
	:config
	(pdf-tools-install)
	(setq-default pdf-view-display-size 'fit-page))

(use-package xkcd
	:config
	(setq xkcd-cache-dir (expand-file-name xkcd-home))
	(setq xkcd-cache-latest (concat xkcd-cache-dir "latest"))
	(general-define-key
	 :states 'normal
	 :keymaps 'xkcd-mode-map
	 "<right>"  #'xkcd-next
	 "h"  #'xkcd-prev
	 "p"  #'xkcd-prev
	 "<left>"  #'xkcd-prev
	 "l"  #'xkcd-next
	 "n"  #'xkcd-next
	 "R"  #'xkcd-rand
	 "q"  #'xkcd-kill-buffer
	 "o"  #'xkcd-open-browser))

;; to add:
;; + telega (telegram)
;; + discord (I don't remember the name)
;; + whatsapp (I'm not sure that package exist)

(provide 'init-app)
