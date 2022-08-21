;; -*- lexical-binding: t -*-

(use-package emacs
	:init
	;; see: https://github.com/lccambiaghi/vanilla-emacs/blob/main/lisp/init-core.el
	(defun google-search-str (str)
		(browse-url
		   (concat "https://www.google.com/search?q=" str)))
	(defun google-search ()
		"Google search region, if active, or ask for search string"
		(interactive)
		(if (region-active-p)
				(google-search-str
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
      (google-search-str (read-from-minibuffer "Search: "))))

	(defun github-code-search ()
		"Search code on github for a given language."
		(interactive)
		(let ((language (completing-read
										 "Language: "
										 '("Emacs Lisp" "Python"  "Clojure" "R" "C" "Bash" "Java")))
					(code (read-string "Code: ")))
			(browse-url
			 (concat "https://github.com/search?l=" language
							 "&type=code&q=" code))))

	(nto/leader-keys
		"s" '(:ignore t :wk "search")
		"sg" '(google-search :wk "google")
		"sc" '(github-code-search :wk "github")
		)
	)

(provide 'init-extra)
