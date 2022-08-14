(use-package org
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

		"t" '(:ignore t :which-key "todo")
		"tt" '(org-todo :wk "heading-todo")
		"ts" '(org-schedule :wk "schedule")
		"td" '(org-deadline :wk "deadline")

		"x" '(org-toggle-checkbox :wk "toggle checkbox"))

	(setq org-src-preserve-indentation t
				org-startup-indented t
				org-catch-invisible-edits 'smart
				org-ellipsis " ⬎ "
				org-image-actual-width nil))

(use-package org-modern
	:after
	(org evil)
	:init
	(global-org-modern-mode))

(use-package org-appear
	:after
	(org evil)
	;; :straight
	;; (org-appear
	;;  :type git
	;;  :host github
	;;  :repo "awth13/org-appear")
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
    (dotimes (_ count) (+org--insert-item 'above)))

  )

;; todo:
;; + add org roam
(use-package org-roam
	:custom
	(org-roam-directory "~/Documents/Roam") ;; todo, move path to a variable
	(org-roam-completion-everywhere t)
	:init
	(setq org-roam-v2-ack t)
	(nto/leader-keys
		"n" '(:ignore t :which-key "Org Roam")
		"na" 'org-roam-aliad-add
		"ng" 'org-roam-graph
		"nc" 'org-roam-capture
		"nt" 'org-roam-tag-add
		"nr" 'org-roam-reg-add
		"nl" 'org-roam-buffer-toggle
		"ni" 'org-roam-node-insert
		"nf" 'org-roam-node-find)
	(nto/local-leader-keys
		;; :states 'normal
		:keymaps '(org-mode-map insert)
		"c" 'completion-at-point)
	:config
	(org-roam-setup))

(provide 'init-org)
