;;; oldt.el --- org-driven development

;; Copyright (C) 2019 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 17 Feb 2019
;; Version: 0.1

;; Keywords: org devtools babel
;; Homepage: https://github.com/rails-to-cosmos/oldt

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package allows you to manage bookmarks and travel around the
;; digital world with an org-mode power behind your shoulders.

;;; Code:

(defun is-there-tags-at-point (tags)
  (seq-intersection tags (org-get-tags-at)))

(defun oldt-get-node-property (property)
  (save-excursion
    (unless (org-at-heading-p)
      (org-back-to-heading))

    (alist-get property (org-entry-properties) nil nil #'string=)))

(defun oldt-search-ancestor (predicate)
  (save-excursion
    (org-with-wide-buffer
     (unless (org-at-heading-p)
       (org-back-to-heading))
     (loop-until (or (funcall predicate)
                     (null (org-up-heading-safe))))
     (when (funcall predicate)
       (point-marker)))))

(defun oldt-project-menu ()
  (interactive)
  (let ((tiny-menu-items
         `(("project-menu"
            (,(oldt-project-get-property "ITEM")
             ((?s "Service" oldt-service-menu)
              (?i "Insert" oldt-project-insert-menu)
              (?p "Pull Request" oldt-project-browse-pull-request)
              (?t "Ticket" oldt-browse-ticket)))))))
    (tiny-menu "project-menu")))

(defun oldt-project-insert-menu ()
  (interactive)
  (let ((tiny-menu-items
         `(("project-insert-menu"
            ("Insert"
             ((?m "Commit message" oldt-project-insert-commit-message)
              (?t "Ticket" oldt-project-insert-ticket)
              (?b "Branch" oldt-project-insert-branch)))))))
    (tiny-menu "project-insert-menu")))

(defun oldt-project-insert-commit-message (msg)
  (interactive "sCommit message: ")
  (oldt-project-insert-ticket)
  (insert ": " msg "."))

(defun oldt-project-insert-ticket ()
  (interactive)
  (insert (oldt-project-get-property "TICKET")))

(defun oldt-project-insert-branch ()
  (interactive)
  (insert (oldt-project-get-property "BRANCH")))

(defun oldt-project-browse-pull-request ()
  (let ((pr-url (oldt-project-get-property "PULL_REQUEST")))
    (browse-url pr-url)))

(defun oldt-tangle-buffer ()
  (org-element-map (org-element-parse-buffer 'element) 'src-block
    (lambda (datum)
      (let* ((lang (org-element-property :language datum))
             (ext (or (cdr (assoc lang org-babel-tangle-lang-exts)) lang))
             (point (org-element-property :begin datum)))
        (org-with-point-at point
          (oldt-tangle-relatives))))))

(defun oldt-tangle-subtree-at-point ()
  (interactive)
  (save-restriction
    (condition-case nil
        (org-narrow-to-subtree)
      (error nil))
    (oldt-tangle-buffer)))

(defun oldt-tangle-project ()
  (interactive)
  (save-excursion
    (oldt-goto-project)
    (oldt-tangle-subtree-at-point)))

(defun oldt-compile-project()
  (interactive)
  (oldt-tangle-project)
  (let ((cmd (oldt-ensure-local-var 'compile-command)))
    (save-excursion
      (oldt-goto-project)
      (save-window-excursion
        (oldt-goto-tangle-file)
        (compile cmd))))

  (switch-to-buffer-other-window "*compilation*"))

(defun oldt-search-project ()
  (cond ((org-at-heading-p) t)
        ((org-clocking-p) (org-clock-goto))
        (t nil))
  (oldt-search-ancestor #'(lambda () (plist-get (org-element--get-node-properties) :CATEGORY))))

(defun oldt-goto-project ()
  (interactive)
  (org-goto-marker-or-bmk (oldt-search-project)))

(defun oldt-project-get-property (property)
  (save-window-excursion
    (save-excursion
      (condition-case nil
          (progn
            (oldt-goto-project)
            (oldt-get-node-property property))
        (error nil)))))

(defun oldt-trigger-function (change-plist)
  (let ((state-from (substring-no-properties (or (plist-get change-plist :from) "")))
        (state-to (substring-no-properties (or (plist-get change-plist :to) ""))))
    (when-let (magic-property (oldt-project-get-property (format "TASK_%s" state-to)))
      (eval (read magic-property)))))
(add-hook 'org-trigger-hook 'oldt-trigger-function)

(defun oldt-task-get-property (property)
  (when (org-clocking-p)
    (save-window-excursion
      (org-clock-goto)
      (if (string= property "STATE")
          (substring-no-properties (org-get-todo-state))
        (org-entry-get (mark) property t)))))

(defun oldt-service-menu ()
  (interactive)
  (let ((tiny-menu-items
         `(("oldt-service"
            (,(oldt-service-get-property "ITEM")
             ((?d "Docker" oldt-docker-menu)
              (?r "Repo" oldt-browse-repo)
              (?c "CI" oldt-service-browse-ci)
              (?l "Logs" oldt-service-browse-logs)))))))
    (tiny-menu "oldt-service")))

(defun oldt-service-get-property (prop)
  (let ((service (split-string (oldt-project-get-property "SERVICES"))))
    (setq service (if (> (length service) 1)
                      (org-completing-read "Service: " service)
                    (car service)))
    (save-window-excursion
      (save-excursion
        (org-id-goto service)
        (alist-get prop (org-entry-properties) nil nil #'string=)))))

(defun oldt-docker-browse-container ()
  (interactive)
  (oldt-goto-project)
  (let ((container (oldt-service-get-property "CONTAINER")))
    (org-open-link-from-string (format "[[docker:%s]]" container))))

(defun oldt-docker-container-logs ()
  (oldt-goto-project)
  (let ((container (oldt-service-get-property "CONTAINER")))
    (org-open-link-from-string (format "[[docker-logs:%s]]" container))))

(defun oldt-docker-compose-config ()
  (let ((path (oldt-service-get-property "PATH")))
    (find-file (concat path "/docker-compose.yml"))))

(defun oldt-docker-compose-up ()
  (let ((path (oldt-service-get-property "PATH")))
    (async-shell-command (format "cd %s && docker-compose up --force-recreate --build -d" path))))

(defun oldt-docker-compose-down ()
  (let ((path (oldt-service-get-property "PATH")))
    (async-shell-command (format "cd %s && docker-compose down && docker image prune -f" path))))

(defun oldt-docker-compose-menu ()
  (interactive)
  (let ((tiny-menu-items
         `(("docker-compose"
            (,(format "%s/docker/compose" (oldt-service-get-property "ITEM"))
             ((?c "Config" oldt-docker-compose-config)
              (?d "Down" oldt-docker-compose-down)
              (?u "Up" oldt-docker-compose-up)))))))
    (tiny-menu "docker-compose")))

(defun oldt-docker-menu ()
  (interactive)
  (let ((tiny-menu-items
         `(("docker"
            (,(format "%s/docker" (oldt-service-get-property "ITEM"))
             ((?b "Browse" oldt-docker-browse-container)
              (?l "Logs" oldt-docker-container-logs)
              (?c "Compose" oldt-docker-compose-menu)))))))
    (tiny-menu "docker")))

(defun oldt-browse-repo ()
  (let ((repo-url (oldt-service-get-property "REPO")))
    (browse-url repo-url)))

(defun oldt-service-browse-logs ()
  (interactive)
  (oldt-goto-project)
  (let ((logs-url (oldt-service-get-property "LOGS")))
    (org-open-link-from-string logs-url)))

(defun oldt-service-browse-ci ()
  (let ((ci-url (oldt-service-get-property "CI")))
    (browse-url ci-url)))

(defun oldt-tangle-relatives (&optional arg target-file lang)
  "Write code blocks to source-specific files.
Extract the bodies of all source code blocks from the current
file into their own source-specific files.
With one universal prefix argument, only tangle the block at point.
When two universal prefix arguments, only tangle blocks for the
tangle file of the block at point.
Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.  Optional argument LANG can be
used to limit the exported source code blocks by language."
  (interactive "P")
  (run-hooks 'org-babel-pre-tangle-hook)
  ;; Possibly Restrict the buffer to the current code block
  (save-restriction
    (save-excursion
      (when (equal arg '(4))
	(if-let (head (org-babel-where-is-src-block-head))
            (goto-char head)
          (user-error "Point is not in a source code block")))
      (let* ((block-counter 0) path-collector

	     (org-babel-default-header-args
	      (if target-file
	          (org-babel-merge-params org-babel-default-header-args
	        			  (list (cons :tangle target-file)))
	        org-babel-default-header-args)))
	(mapc ;; map over all languages
	 (lambda (by-lang)
	   (let* ((lang (car by-lang))
		  (specs (cdr by-lang))
		  (ext (or (cdr (assoc lang org-babel-tangle-lang-exts)) lang))
		  (lang-f (intern
			   (concat
			    (or (and (cdr (assoc lang org-src-lang-modes))
				     (symbol-name
				      (cdr (assoc lang org-src-lang-modes))))
				lang)
			    "-mode")))
		  she-banged)
	     (mapc
	      (lambda (spec)
		(let ((get-spec (lambda (name) (cdr (assoc name (nth 4 spec))))))
		  (let* ((tangle (funcall get-spec :tangle))
			 (she-bang (let ((sheb (funcall get-spec :shebang)))
                                     (when (> (length sheb) 0) sheb)))
			 (tangle-mode (funcall get-spec :tangle-mode))
                         (tangle-dir (get-tangle-dir-at-point))
			 (base-name (cond
				     ((string= "yes" tangle)
				      (file-name-sans-extension
				       (nth 1 spec)))
				     ((string= "no" tangle) nil)
				     ((> (length tangle) 0) tangle)))
			 (file-name (consider-tangle-dir
                                     (when base-name
				       ;; decide if we want to add ext to base-name
				       (if (and ext (string= "yes" tangle))
					   (concat base-name "." ext) base-name)))))
		    (when file-name
		      ;; Possibly create the parent directories for file.
		      (let ((m (funcall get-spec :mkdirp))
			    (fnd (file-name-directory file-name)))
			(and m fnd (not (string= m "no"))
			     (make-directory fnd 'parents)))
		      ;; delete any old versions of file
		      (and (file-exists-p file-name)
			   (not (member file-name (mapcar #'car path-collector)))
			   (delete-file file-name))
		      ;; drop source-block to file
		      (with-temp-buffer
			(when (fboundp lang-f) (ignore-errors (funcall lang-f)))
			(when (and she-bang (not (member file-name she-banged)))
			  (insert (concat she-bang "\n"))
			  (setq she-banged (cons file-name she-banged)))
			(org-babel-spec-to-string spec)
			;; We avoid append-to-file as it does not work with tramp.
			(let ((content (buffer-string)))
			  (with-temp-buffer
			    (when (file-exists-p file-name)
			      (insert-file-contents file-name))
			    (goto-char (point-max))
			    ;; Handle :padlines unless first line in file
			    (unless (or (string= "no" (cdr (assq :padline (nth 4 spec))))
					(= (point) (point-min)))
			      (insert "\n"))
			    (insert content)
			    (write-region nil nil file-name))))
		      ;; if files contain she-bangs, then make the executable
		      (when she-bang
			(unless tangle-mode (setq tangle-mode #o755)))
		      ;; update counter
		      (setq block-counter (+ 1 block-counter))
		      (unless (assoc file-name path-collector)
			(push (cons file-name tangle-mode) path-collector))))))
	      specs)))
         (oldt-collect-relative-blocks))

	;; run `org-babel-post-tangle-hook' in all tangled files
	(when org-babel-post-tangle-hook
	  (mapc
	   (lambda (file)
	     (org-babel-with-temp-filebuffer file
	       (run-hooks 'org-babel-post-tangle-hook)))
	   (mapcar #'car path-collector)))
	;; set permissions on tangled files
	(mapc (lambda (pair)
		(when (cdr pair) (set-file-modes (car pair) (cdr pair))))
	      path-collector)

        (message "Tangled %d code block%s from %s to %s" block-counter
		 (if (= block-counter 1) "" "s")
		 (file-name-nondirectory
		  (buffer-file-name
		   (or (buffer-base-buffer) (current-buffer))))
                 (caar path-collector))
        path-collector))))

(defun consider-tangle-dir (file-name)
  (if-let (tangle-dir (get-tangle-dir-at-point))
      (when (and file-name (f-relative-p file-name))
        (f-join tangle-dir file-name))
    file-name))

(defun get-tangle-dir-at-point ()
  (if (org-before-first-heading-p)
      ""
    (save-excursion
      (cl-loop initially (org-back-to-heading)
               with tangle-dir-at-point = (lambda () (plist-get (org-element--get-node-properties) :TANGLE_DIR))
               with tangle-dir = (when-let (tangle-dir (funcall tangle-dir-at-point))
                                   (list tangle-dir))
               for level = (org-up-heading-safe)
               for dir = (funcall tangle-dir-at-point)
               when (and level dir) collect dir into tangle-dir
               unless level return (when tangle-dir (apply 'f-join (reverse tangle-dir)))))))

(defun oldt-collect-relative-blocks ()
  (let* ((counter 0) last-heading-pos blocks
         (info (org-babel-get-src-block-info 'light))
         (babel-params (nth 2 info))
         (src-tfile (consider-tangle-dir (alist-get :tangle babel-params)))
         (src-lang (car info)))

    (org-babel-map-src-blocks (buffer-file-name)
      (unless (org-in-commented-heading-p)
        (let* ((info (org-babel-get-src-block-info 'light))
               (params (nth 2 info))
               (tangle-file (consider-tangle-dir (alist-get :tangle params)))
               (block (unless (or (string= src-tfile "no")
		                  (and tangle-file (not (equal tangle-file src-tfile))))
                        (cl-incf counter)
                        (org-babel-tangle-single-block counter))))
          (push (cons src-lang (list block)) blocks))))

    ;; Ensure blocks are in the correct order.
    (nreverse blocks)))

(defun oldt-collect-tangle-files-in-buffer ()
  (-distinct
   (-flatten
    (org-element-map (org-element-parse-buffer 'element) 'src-block
      (lambda (datum)
        (let* ((lang (org-element-property :language datum))
               (ext (or (cdr (assoc lang org-babel-tangle-lang-exts)) lang))
               (point (org-element-property :begin datum)))

          (org-with-point-at point
            (let* ((props (org-babel-params-from-properties lang))
                   (args (mapcar #'org-babel-parse-header-arguments
	                         (cons (org-element-property :parameters datum)
	                               (org-element-property :header datum))))
                   (blocks (-flatten (append props args))))
              (loop for (key . value) in blocks
                    when (eq key :tangle)

                    if (string= value "yes")
                    collect (expand-file-name
                             (consider-tangle-dir (concat
                                                   (file-name-sans-extension
                                                    (buffer-file-name)) "." ext)))

                    else unless (string= value "no")
                    collect (expand-file-name
                             (consider-tangle-dir value)))))))))))

(defun oldt-collect-tangle-files-in-subtree ()
  (interactive)
  (save-restriction
    (condition-case nil
        (org-narrow-to-subtree)
      (error nil))
    (oldt-collect-tangle-files-in-buffer)))

(defun oldt-collect-project-tangle-files ()
  (save-excursion
    (oldt-goto-project)
    (oldt-collect-tangle-files-in-subtree)))

(defun oldt-goto-tangle-file()
  (interactive)
  (if-let (tangle-files (oldt-collect-tangle-files-in-subtree))
      (switch-to-buffer
       (find-file-noselect
        (if (> (length tangle-files) 1)
            (org-completing-read "Choose file to visit: " tangle-files)
          (car tangle-files))
        t))
    (unless tangle-files
      (error "No tangle files all the way down"))))

(defun files-in-below-directory (directory)
  "List the .el files in DIRECTORY and in its sub-directories."
  ;; Although the function will be used non-interactively,
  ;; it will be easier to test if we make it interactive.
  ;; The directory will have a name such as
  ;;  "/usr/local/share/emacs/22.1.1/lisp/"
  (interactive "DDirectory name: ")
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename ends in '.el'
       ;; and if so, add its name to a list.
       ((equal ".el" (substring (car (car current-directory-list)) -3))
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()
          ;; else descend into the directory and repeat the process
          (setq el-files-list
                (append
                 (files-in-below-directory
                  (car (car current-directory-list)))
                 el-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    el-files-list))

(defun oldt-ensure-local-var(symbol)
  (unless (and (boundp symbol) (local-variable-p symbol))
    (let ((value (read-string (format "%s: " (symbol-name symbol)))))
      (add-file-local-variable symbol value)))
  (eval symbol))

(defun oldt-build ()
  (interactive)
  (let ((project-files (files-in-below-directory "./")))
    (org-babel-tangle)
    (mapc 'load-file project-files)
    (mapc 'byte-compile-file project-files)
    (mapc 'oldt-ensure-local-var
          '(org-literate-test-selector org-literate-test-buffer))

    (let* ((ert-stats (ert-run-tests-interactively org-literate-test-selector org-literate-test-buffer))
           (expected (ert-stats-completed-expected ert-stats))
           (unexpected (ert-stats-completed-unexpected ert-stats))
           (skipped (ert-stats-skipped ert-stats))
           (total (ert-stats-total ert-stats))
           (report (list "Build finished. Ran %d tests, %d were as expected, %d failed, %d skipped"
                         total expected unexpected skipped)))
      (apply 'message report))))

(defun oldt-magit-workon ()
  "Switch to project branch."
  (interactive)
  (save-window-excursion
    (save-excursion
      (org-clock-goto)
      (let* ((repo (oldt-project-get-property "REPO"))
             (branch (oldt-project-get-property "BRANCH"))
             (source (oldt-project-get-property "SOURCE_BRANCH"))
             (default-directory (concat "~/work/" repo)))
        (unless (string= branch (magit-get-current-branch))
          (when (y-or-n-p "Switch to task branch?")
            (magit-branch-or-checkout branch source)
            (magit-branch-checkout branch)))))))

(defun oldt-insert-commit-msg ()
  (insert (oldt-project-get-property "BRANCH") ": " (oldt-task-get-property "ITEM")))

(defun oldt-task-trigger-todo-hook ()
  (interactive)
  (let ((state (oldt-task-get-property "STATE"))
        (default-directory (file-name-directory (buffer-file-name (org-clocking-buffer)))))
    (oldt-trigger-function (list :from state :to "TODO"))))

(defun oldt-task-trigger-start-hook ()
  (interactive)
  (let ((state (oldt-task-get-property "STATE"))
        (default-directory (file-name-directory (buffer-file-name (org-clocking-buffer)))))
    (oldt-trigger-function (list :from "TODO" :to state))))

(defun oldt-browse-ticket ()
  (save-window-excursion
    (save-excursion
      (org-save-outline-visibility
          (when-let ((ticket (oldt-project-get-property "TICKET")))
            (oldt-goto-project)
            (when-let ((ticket-link (alist-get "ticket" org-link-abbrev-alist-local nil nil #'string=)))
              (browse-url (format ticket-link ticket))))))))

(defun oldt-send-current-to-remote ()
  (interactive)
  (when (and (boundp 'oldt-source-dir)
             (boundp 'oldt-target-dir))
    (copy-file (buffer-file-name)
               (s-replace oldt-source-dir oldt-target-dir (buffer-file-name))
               t)))

(add-hook 'after-save-hook 'oldt-send-current-to-remote)

(defun oldt-evaluate-blocks-current-heading ()
  (org-back-to-heading)
  (save-excursion
    (save-restriction
      (org-save-outline-visibility nil
        (narrow-to-region (org-entry-beginning-position) (org-entry-end-position))
        (loop while (condition-case-unless-debug user-error (org-babel-next-src-block) (user-error nil))
              collect (org-babel-execute-src-block nil nil '((:results . "silent"))) into report
              finally (return (-all-p (lambda (result) (s-contains-p "success" (downcase result))) report)))))))

(defun oldt-heading-sbe ()
  (interactive)
  (ledna/set-todo-state "LOADING")
  (sit-for 0.2)
  (if (oldt-evaluate-blocks-current-heading)
      (ledna/set-todo-state "PASSED")
    (ledna/set-todo-state "FAILED")))

(defun oldt-reset-tests ()
  (interactive)
  (ledna/set-todo-state "TEST" (ledna/search ":oldt:test_case:" 'tree))
  (org-update-statistics-cookies t))

(provide 'org-literate-devtools)
;;; org-literate-devtools.el ends here
