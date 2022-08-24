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
 	 (kbd "M") 'dired-chmod
 	 (kbd "O") 'dired-chown
 	 (kbd "R") 'dired-do-rename
 	 (kbd "T") 'dired-do-touch
 	 (kbd "Y") 'dired-copy-filename-as-kill ; copies filename to kill ring.
 	 (kbd "+") 'dired-create-directory
 	 (kbd "-") 'dired-up-directory
 	 (kbd "% l") 'dired-downcase
 	 (kbd "% u") 'dired-upcase
 	 (kbd "; d") 'epa-dired-do-decrypt
 	 (kbd "; e") 'epa-dired-do-encrypt)
	(setq dired-omit-files "^\\.[^.]\\|$Rhistory\\|$RData\\|__pycache__")
	(setq dired-listing-switches "-lah")
  (setq ls-lisp-dirs-first t)
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq dired-dwim-target t)
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
(use-package dirvish
	:if (not nto/is-raspberry)
	:init
	(nto/leader-keys
		;; "." 'dirvish-side
		;; "fj" 'dirvish)
		"fd" '(:ignore t :which-key "dirvish")
		"fds" 'dirvish-side
		"fd." 'dirvish)
	;; (dirvish-override-dired-mode)
	(setq dirvish-peek-mode t)
	:custom
	(dirvish-attributes '(file-size collapse subtree-state vc-state git-msg)))

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
