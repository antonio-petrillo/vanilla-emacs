;; -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((file-name-handler-alist nil)
      (gc-cons-threshold 100000000))
  (require 'init-core)
  (require 'init-extra)
  (require 'init-ui)
  (require 'init-app)
  (require 'init-org)
  (require 'init-prog))
