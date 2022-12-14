;; -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "exwm" user-emacs-directory))

;; (if (and (eq system-type 'gnu/linux)
;; 				 (string-match "x86_64" (shell-command-to-string "uname -m"))
;; 				 t)
;; 		(add-to-list 'load-path (expand-file-name "exwm" user-emacs-directory)))

(let ((file-name-handler-alist nil)
      (gc-cons-threshold 100000000))

  (require 'init-core)
  (require 'init-extra)
  (require 'init-ui)
  (require 'init-app)
  (require 'init-prog)
  (require 'init-org))
