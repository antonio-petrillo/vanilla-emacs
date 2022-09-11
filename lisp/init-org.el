;; -*- lexical-binding: t -*-

(defun nto/org-insert-checkbox-below ()
	"Insert a checkbox at point."
	(interactive)
	(evil-open-below 1)
	(insert "+ [ ] "))

(defun nto/org-insert-checkbox-above ()
	"Insert a checkbox at point."
	(interactive)
	(evil-open-above 1)
	(insert "+ [ ] "))

(use-package org
	:straight (:type built-in)
	:hook
	((org-mode . prettify-symbols-mode)
	 (org-mode . visual-line-mode))
	:init
	(nto/local-leader-keys
		:keymaps '(org-mode-map)
		"f" '(:ignore t :which-key "file")
		"ft" '(org-babel-tangle :wk "tangle")

		"i" '(:ignore t :which-key "insert")
		"il" '(org-insert-link :wk "link")
		"ic" '(nto/org-insert-checkbox-below :wk "checkbox")
		"iC" '(nto/org-insert-checkbox-above :wk "checkbox")

		"t" '(:ignore t :which-key "todo")
		"tt" '(org-todo :wk "heading-todo")
		"ts" '(org-schedule :wk "schedule")
		"td" '(org-deadline :wk "deadline")

		"x" '(org-toggle-checkbox :wk "toggle checkbox")
		"c" 'org-ctrl-c-ctrl-c)

	(setq org-src-preserve-indentation t
				org-startup-indented t
				org-catch-invisible-edits 'smart
				org-ellipsis " ⬎ "
				org-image-actual-width nil)

	(setq org-log-done t)

	(setq org-agenda-files '("~/Documents/Org/Agenda/Notes.org"
													 "~/Documents/Org/Agenda/Private.org"
													 "~/Documents/Org/Agenda/Uni.org"))
	(nto/leader-keys
		"oa" '(:ignore t :wk "Agenda")
		"oaa" 'org-agenda
		"oan" '((lambda () (interactive) (find-file (nth 0 org-agenda-files))) :wk "Note agenda")
		"oap" '((lambda () (interactive) (find-file (nth 1 org-agenda-files))) :wk "Private agenda")
		"oau" '((lambda () (interactive) (find-file (nth 2 org-agenda-files))) :wk "Uni agenda")))

(use-package org-modern
	:if (not nto/is-raspberry)
	:after
	(org evil)
	:init
	(global-org-modern-mode))

(use-package org-appear
	:if (not nto/is-raspberry)
	:after
	(org evil)
	:hook
	(org-mode . org-appear-mode)
	:init
	(setq org-appear-autoentities t)
	(setq org-appear-autokeywords t)
	(setq org-appear-inside-latex t)
	(setq org-appear-autoemphasis t)
	(setq org-appear-autolinks t)
	(setq org-appear-trigger 'always)
	(setq org-appear-autosubmarkers t))

(use-package org-fragtog
	:if (not nto/is-raspberry)
	:after
	(org evil)
	:hook
	(org-mode . org-fragtog-mode))

(use-package evil-org-mode
  :straight (evil-org-mode :type git :host github :repo "hlissner/evil-org-mode")
  :hook ((org-mode . evil-org-mode)
         (org-mode . (lambda ()
                       (require 'evil-org)
                       (evil-normalize-keymaps)
                       (evil-org-set-key-theme '(textobjects))
                       (require 'evil-org-agenda)
                       (evil-org-agenda-set-keys))))
  :bind
  ([remap evil-org-org-insert-heading-respect-content-below] . +org/insert-item-below) ;; "<C-return>"
  ([remap evil-org-org-insert-todo-heading-respect-content-below] . +org/insert-item-above) ;; "<C-S-return>"
  :general
  (general-nmap
    :keymaps 'org-mode-map
    :states 'normal
    "RET"   #'org-open-at-point
    ;; "RET"   #'+org/dwim-at-point
		)
  :init
  (defun +org--insert-item (direction)
    (let ((context (org-element-lineage
                    (org-element-context)
                    '(table table-row headline inlinetask item plain-list)
                    t)))
      (pcase (org-element-type context)
        ;; Add a new list item (carrying over checkboxes if necessary)
        ((or `item `plain-list)
         ;; Position determines where org-insert-todo-heading and org-insert-item
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (org-end-of-item)
           (backward-char))
         (org-insert-item (org-element-property :checkbox context))
         ;; Handle edge case where current item is empty and bottom of list is
         ;; flush against a new heading.
         (when (and (eq direction 'below)
                    (eq (org-element-property :contents-begin context)
                        (org-element-property :contents-end context)))
           (org-end-of-item)
           (org-end-of-line)))

        ;; Add a new table row
        ((or `table `table-row)
         (pcase direction
           ('below (save-excursion (org-table-insert-row t))
                   (org-table-next-row))
           ('above (save-excursion (org-shiftmetadown))
                   (+org/table-previous-row))))

        ;; Otherwise, add a new heading, carrying over any todo state, if
        ;; necessary.
        (_
         (let ((level (or (org-current-level) 1)))
           ;; I intentionally avoid `org-insert-heading' and the like because they
           ;; impose unpredictable whitespace rules depending on the cursor
           ;; position. It's simpler to express this command's responsibility at a
           ;; lower level than work around all the quirks in org's API.
           (pcase direction
             (`below
              (let (org-insert-heading-respect-content)
                (goto-char (line-end-position))
                (org-end-of-subtree)
                (insert "\n" (make-string level ?*) " ")))
             (`above
              (org-back-to-heading)
              (insert (make-string level ?*) " ")
              (save-excursion (insert "\n"))))
           (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                       (todo-type    (org-element-property :todo-type context)))
             (org-todo
              (cond ((eq todo-type 'done)
                     ;; Doesn't make sense to create more "DONE" headings
                     (car (+org-get-todo-keywords-for todo-keyword)))
                    (todo-keyword)
                    ('todo)))))))

      (when (org-invisible-p)
        (org-show-hidden-entry))
      (when (and (bound-and-true-p evil-local-mode)
                 (not (evil-emacs-state-p)))
        (evil-insert 1))))

  (defun +org/insert-item-below (count)
    "Inserts a new heading, table cell or item below the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'below)))

  (defun +org/insert-item-above (count)
    "Inserts a new heading, table cell or item above the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'above))))

(use-package denote
	:init
	(setq denote-directory (expand-file-name "~/Documents/Org/Denote"))
	(setq denote-known-keywords '("journal" "projects" "ideas" "knowledge" "emacs" "linux" "3d-print" "keyboard" "book" "game"))
	(setq denote-file-type nil)
	(add-hook 'dired-mode-hook #'denote-dired-mode)

	(defun nto/try-hack-me ()
"Prompt for denote 'title', 'keywords' then auto-add 'tryhackme' keyword.
Useful for keep track of my learning path on tryhackme platform"
		(interactive)
		(let ((title (denote--title-prompt))
					(keywords (denote--keywords-prompt))
					(tryhackme-home (expand-file-name "TryHackMe" denote-directory)))
			(if (not (member "tryhackme" keywords))
					(push "tryhackme" keywords))
			(denote
			 title
			 keywords
			 nil
			 tryhackme-home)))

	(defun nto/denote-journal-with-keyword ()
	  "Prompt for denote 'keywords' and create a journal entry (by calling 'denote-journal')"
		(interactive)
		(let ((keywords (denote--keywords-prompt)))
			(nto/denote-journal keywords)))

	(defun nto/denote-journal (&optional keywords)
		"Create an entry tagged 'journal' and the other 'keywords' with the date as its title, there will be only one entry per day."
		(interactive)

		(let* ((journal-home (expand-file-name "Journal" denote-directory))
					 (formatted-date (format-time-string "%A %e %B %Y"))
					 (entry-of-today-regex (mapconcat 'downcase (remove "" (split-string formatted-date " ")) "-" ))
					 ;; (entry-of-today-regex (downcase (format-time-string "%A-%e-%B-%Y")))
					 ;; don't work, day with one digit (like 1 September) generate wrong regex (like 'Wednesday--1-September-2022' instead of 'Wednesday--1-September-2022')
					 (entry-of-today (car (directory-files journal-home nil entry-of-today-regex)))
					 )

			(if (not (member "journal" keywords))
					(push "journal" keywords))
			(if entry-of-today
					(find-file (expand-file-name entry-of-today journal-home))
				(denote
				 formatted-date
				 keywords
				 nil
				 journal-home)
				(insert "* Thoughts\n\n* Tasks\n\n"))))

	(defun nto/goto-denote-directory ()
		(interactive)
		(find-file denote-directory))

	(nto/leader-keys
		"n" '(:ignore t :wk "Denote")
		"nn" 'denote ;; create new note
		"nf" 'nto/goto-denote-directory ;; select referenced note in minibuffer
		"nF" 'denote-link-find-file ;; select referenced note in minibuffer
		"nl" 'denote-link
		"nL" 'denote-link-add-links ;; add link to all file matching REGEX
		"nb" 'denote-link-backlinks ;; produce buffer with files linking to current note
		"nj" 'nto/denote-journal ;; journaling with denote
		"nJ" 'nto/denote-journal-with-keyword ;; journaling with denote
		"nt" 'nto/try-hack-me
		"ns" '(:ignore t :wk "Search Denote")
		"nsj" '((lambda () (interactive) (find-file (expand-file-name  "Journal" denote-directory))) :wk "Journal")
		"nst" '((lambda () (interactive) (find-file (expand-file-name  "TryHackMe" denote-directory))) :wk "TryHackMe")
		))

(provide 'init-org)
