;; -*- lexical-binding: t; -*-

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

(require 'aio)
(require 'ert)
(require 'org)
(require 'org-capture)

(defun oldt-headline-contains-tags-p (&rest tags)
  (equal (seq-intersection tags (org-get-tags)) tags))

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

(setq oldt-special-symbols
      `((,(unibyte-string 226 149 152) . "╘")
        (,(unibyte-string 226 149 146) . "╒")
        (,(unibyte-string 226 149 158) . "╞")
        (,(unibyte-string 226 149 167) . "╧")
        (,(unibyte-string 226 149 170) . "╪")
        (,(unibyte-string 226 149 155) . "╛")
        (,(unibyte-string 226 149 161) . "╡")
        (,(unibyte-string 226 148 130) . "│")
        (,(unibyte-string 226 149 149) . "╕")
        (,(unibyte-string 226 149 164) . "╤")
        (,(unibyte-string 226 149 144) . "═")))

(setq oldt-special-faces
      `(("\\(\\w*INFO\\w*\\)" . compilation-info)
        ("\\(\\w*WARN\\w*\\)" . compilation-warning)
        ("\\(\\w*CRIT\\w*\\)" . compilation-error)
        ("\\(\\w*FATAL\\w*\\)" . compilation-error)))

(defun oldt-beautify-process-output (process coding-string)
  (let ((buffer (process-buffer process))
        (message (encode-coding-string (decode-coding-string coding-string 'mac) 'unix)))
    (with-current-buffer buffer
      (loop for line in (split-string message "\n")
            do (save-excursion
                 (let ((beg (save-excursion
                              (beginning-of-line)
                              (point))))
                   (goto-char (point-max))
                   (insert (string-trim line))
                   (when (not (eq beg (point-max)))
                     (loop for rt in oldt-special-symbols
                           do (save-excursion
                                (goto-char beg)
                                (while (search-forward (car rt) nil t)
                                  (replace-match (cdr rt)))))

                     (loop for rt in oldt-special-faces
                           do (save-excursion
                                (goto-char beg)
                                (when (re-search-forward (car rt) (point-max) t)
                                  (let ((beg (match-beginning 0))
                                        (end (match-end 0)))
                                    (add-face-text-property beg end (cdr rt) t (current-buffer))))))

                     (goto-char (point-max))
                     (ansi-color-apply-on-region beg (point-max))
                     (insert "\n"))))))))

(aio-defun oldt-start-process-async (pname buf &rest args)
  (let* ((proc (make-process
                :name pname
                :buffer buf
                :command args
                :coding '(mac . unix)
                :filter #'oldt-beautify-process-output)))
    (while (string= (process-status proc) "run")
      (aio-await (aio-sleep 0.1)))
    (process-exit-status proc)))

(defun oldt-at-project-p ()
  (save-excursion
    (org-back-to-heading)
    (org-beginning-of-line)
    (plist-get (org-element--get-node-properties) :PROJECT)))

(defun oldt-project-menu ()
  (interactive)
  (let ((items '("service-eshell"
                 "service-browse-repo"
                 "service-browse-deploy"
                 "service-browse-logs"
                 "service-browse-url"

                 "service-docker-compose-config"
                 "service-docker-compose-down"
                 "service-docker-compose-build"
                 "service-docker-compose-restart"
                 "service-docker-compose-up"
                 "service-docker-container-dired"
                 "service-docker-container-eshell"
                 "service-docker-container-logs"

                 "project-browse-ticket"
                 "project-insert-ticket"
                 "project-insert-branch"

                 "jira-get-ticket-worklog"
                 "jira-update-project-status"

                 "task-narrow"
                 "task-insert-commit-message"
                 "task-browse-pull-request")))
    (if-let (project-name (oldt-project-get-property "ITEM"))
        (-some->> items
                  (org-completing-read (concat project-name ": "))
                  (concat "oldt-")
                  (intern)
                  (funcall))
      (user-error "Unable to find project."))))

(defun oldt-project-insert-ticket ()
  (interactive)
  (insert (oldt-project-get-property "TICKET")))

(defun oldt-project-insert-branch ()
  (interactive)
  (insert (oldt-project-get-property "BRANCH")))

(defun oldt-tangle-buffer ()
  (org-element-map (org-element-parse-buffer 'element) 'src-block
    (lambda (datum)
      (let ((point (org-element-property :begin datum)))
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
  (cond ((and (org-at-heading-p) (oldt-at-project-p)) t)
        ((org-clocking-p) (progn
                            (org-clock-goto)
                            (org-beginning-of-line)
                            t))
        (t nil))
  (oldt-search-ancestor 'oldt-at-project-p))

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

(defun oldt-project-set-property (property value)
  (save-window-excursion
    (save-excursion
      (oldt-goto-project)
      (cond ((string= property "ITEM")
             (let ((beg (save-excursion
                          (org-beginning-of-line)
                          (point)))
                   (end (save-excursion
                          (org-end-of-line)
                          (point))))
               (kill-region beg end)
               (org-beginning-of-line)
               (insert value)))
            ((string= property "TODO_STATE")
             (org-todo value))
            (t (org-set-property property value))))))

(defun oldt-narrow-to-project ()
  (interactive)
  (widen)
  (oldt-goto-project)
  (org-narrow-to-subtree)
  (org-content))

(defun oldt-project-browse-ticket ()
  (save-window-excursion
    (save-excursion
      (org-save-outline-visibility
          (when-let ((ticket (oldt-project-get-property "TICKET")))
            (oldt-goto-project)
            (when-let ((ticket-link (alist-get "ticket" org-link-abbrev-alist-local nil nil #'string=)))
              (browse-url (format ticket-link ticket))))))))

(defun oldt-task-narrow ()
  (interactive)
  (oldt-goto-task)
  (org-narrow-to-subtree)
  (goto-char (point-max)))

(defun oldt-task-browse (property)
  (if-let (val (-> property
                   oldt-task-get-property
                   split-string))
      (if (> (length val) 1)
          (org-link-open-from-string
           (org-completing-read (format "Browse %s: " property) val))
        (org-link-open-from-string (car val)))))

(defun oldt-at-task-p ()
  (save-excursion
    (when (condition-case nil
              (org-back-to-heading)
            (error nil))
      (org-beginning-of-line)
      (not (oldt-at-project-p)))))

(defun oldt-task-insert-commit-message ()
  (let ((msg (read-string "Commit message: " (oldt-task-get-property "ITEM"))))
    (insert (concat (oldt-project-get-property "TICKET") ": "))
    (insert msg)
    (unless (s-ends-with-p "." msg)
      (insert "."))))

(defun oldt-task-browse-pull-request ()
  (cond ((oldt-at-task-p) (oldt-task-browse "PULL_REQUEST"))
        ((oldt-at-project-p)
         (save-restriction
           (org-narrow-to-subtree)
           (org-element-map (org-element-parse-buffer) 'headline
             (lambda (hl) (let ((property (substring-no-properties (org-element-property :todo-keyword hl))))
                       (when (string= property "CODE_REVIEW")
                         (save-excursion
                           (goto-char (org-element-property :begin hl))
                           (when-let ((pr (plist-get (org-element--get-node-properties) :PULL_REQUEST)))
                             (browse-url pr)))))))))
        (t (oldt-goto-project)
           (oldt-task-browse-pull-request))))

(defun oldt-set-pull-request-if-not-specified ()
  (when (oldt-at-task-p)
    (unless (oldt-task-get-property "PULL_REQUEST")
      (-some->> (org-read-property-value "PULL_REQUEST")
                (oldt-task-set-property "PULL_REQUEST")))
    (when (org-clocking-p)
      (let ((org-clock-out-switch-to-state "CODE_REVIEW"))
        (org-clock-out)))))

(defun oldt-search-task ()
  (if (cond ((oldt-at-task-p) t)
            ((org-clocking-p) (progn
                                (org-clock-goto)
                                (oldt-at-task-p)))
            (t nil))
      (point-marker)
    (error "Task not found.")))

(defun oldt-goto-task ()
  (interactive)
  (let ((mark (oldt-search-task)))
    (org-goto-marker-or-bmk mark)
    mark))

(defun oldt-task-set-property (property value)
  (save-window-excursion
    (save-excursion
      (oldt-goto-task)
      (cond ((string= property "ITEM")
             (let ((beg (save-excursion
                          (org-beginning-of-line)
                          (point)))
                   (end (save-excursion
                          (org-end-of-line)
                          (point))))
               (kill-region beg end)
               (org-beginning-of-line)
               (insert value)))
            (t (org-set-property property value))))))

(defun oldt-trigger-function (change-plist)
  (let (;; (state-from (substring-no-properties (or (plist-get change-plist :from) "")))
        (state-to (substring-no-properties (or (plist-get change-plist :to) ""))))
    (when-let (magic-property (oldt-project-get-property (format "TASK_%s" state-to)))
        (progn
          (message "Evaluating %s" magic-property)
          (oldt-goto-task)
          (eval (read magic-property))))))
(add-hook 'org-trigger-hook 'oldt-trigger-function)

(defun oldt-task-get-property (property)
  (save-window-excursion
    (save-excursion
      (let ((marker (oldt-goto-task)))
        (if (string= property "STATE")
            (substring-no-properties (org-get-todo-state))
          (org-entry-get marker property t))))))

(defun oldt-service-eshell ()
  (spawn-custom-shell (format "*%s-eshell*" (oldt-service-get-property "ITEM"))
                      (oldt-service-get-property "PATH")))

(defun oldt-service-get-property (property)
  (let ((service (split-string (oldt-project-get-property "SERVICES"))))
    (setq service (if (> (length service) 1)
                      (org-completing-read "Service: " service)
                    (car service)))
    (save-window-excursion
      (save-excursion
        (org-id-goto service)
        (oldt-get-node-property property)))))

(defun oldt-service-docker-system-prune ()
  (interactive)
  (async-shell-command "docker system prune -a --volumes"))

(defun oldt-service-docker-container-dired ()
  (interactive)
  (oldt-goto-project)
  (let ((container (oldt-service-get-property "CONTAINER")))
    (org-link-open-from-string (format "[[docker:%s]]" container))))

(defun oldt-service-docker-container-logs ()
  (interactive)
  (let* ((container (oldt-service-get-property "CONTAINER"))
         (cmd (format "docker logs %s -f" container))
         (bufname (format "*docker-%s-logs*" container)))
    (get-buffer-create bufname)
    (async-shell-command cmd bufname)
    (switch-to-buffer-other-window bufname)
    (goto-char (point-max))
    ;; (special-mode)
    ;; (auto-revert-mode)
    ))

(defun oldt-service-docker-container-eshell ()
  (interactive)
  (let ((container (oldt-service-get-property "CONTAINER"))
        (service (oldt-service-get-property "ITEM")))
    (spawn-custom-shell (format "*%s-docker-container-eshell*" service)
                        (format "/docker:%s:/opt" container))))

;; (defun oldt-service-docker-container-logs ()
;;   (interactive)
;;   (oldt-goto-project)
;;   (let ((container (oldt-service-get-property "CONTAINER")))
;;     (org-link-open-from-string (format "[[docker-logs:%s]]" container))))

(defun oldt-service-docker-compose-config ()
  (let ((path (oldt-service-get-property "PATH")))
    (find-file (concat path "/docker-compose.yml"))))

(defun oldt-load-env-file (env-file-name)
  (loop for env in (read-lines env-file-name)
        collect (let ((uncomenv (car (s-split-up-to "#" env 1))))
                  (pcase-let ((`(,key ,value) (s-split-up-to "=" uncomenv 1)))
                    (let ((val (->> value
                                    (s-chop-prefix "\"")
                                    (s-chop-suffix "\""))))
                      (format "%s=%s" key val))))))

(aio-defun oldt-service-start-process (pname buf &rest args)
  (let ((service (oldt-service-get-property "ITEM")))
    (message "%s: Start process \"%s\"" service pname)
    (let* ((default-directory (oldt-service-get-property "PATH"))
           (process-environment (append process-environment (oldt-load-env-file ".env.local"))))
      ;; (message "Using extended process-environment:")
      ;; (loop for env in process-environment
      ;;       do (message env))
      (aio-await (apply #'oldt-start-process-async pname buf args)))))

(aio-defun oldt-service-docker-compose-down ()
  (aio-await (oldt-service-start-process "docker-compose down" "*oldt-service-docker-output*" "docker-compose" "down")))

(aio-defun oldt-service-docker-compose-build ()
  (aio-await (oldt-service-start-process "docker-compose build app" "*oldt-service-docker-output*" "docker-compose" "build" "app")))

(aio-defun oldt-service-docker-compose-up ()
  (aio-await (oldt-service-start-process "docker-compose up" "*oldt-service-docker-output*" "docker-compose" "up")))

(aio-defun oldt-service-docker-compose-restart ()
  (aio-await (oldt-service-docker-compose-down))
  (aio-await (oldt-service-docker-compose-build))
  (aio-await (oldt-service-docker-compose-up)))

(defun oldt-service-browse-repo ()
  (when-let ((repo-url (oldt-service-get-property "REPO")))
    (org-link-open-from-string repo-url)))

(defun oldt-service-browse-logs ()
  (interactive)
  (oldt-goto-project)
  (let ((logs-url (oldt-service-get-property "LOGS")))
    (org-link-open-from-string logs-url)))

(defun oldt-service-browse-deploy ()
  (loop for url in (split-string (oldt-service-get-property "CI"))
        do (org-link-open-from-string url)))

(defun oldt-service-browse-url ()
  (let ((property "URL"))
    (if-let (val (-> property
                   oldt-service-get-property
                   split-string))
      (if (> (length val) 1)
          (org-link-open-from-string
           (org-completing-read (format "Browse %s: " property) val))
        (org-link-open-from-string (car val))))))

(defun oldt-tt (&rest mappings)
  (loop for mapping in mappings
        when (or (eq (car mapping) t) ;; "else" clause
                 (apply 'oldt-headline-contains-tags-p (butlast mapping)))
        collect (car (last mapping)) into result
        finally (return (if result (car result) "no"))))

(defun oldt-tangle-relatives (&optional arg target-file &rest _)
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
  (let* ((counter 0) blocks
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

    (let* ((org-literate-test-selector (oldt-ensure-local-var 'org-literate-test-selector))
           (org-literate-test-buffer (oldt-ensure-local-var 'org-literate-test-buffer))
           (ert-stats (ert-run-tests-interactively org-literate-test-selector org-literate-test-buffer))
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
      (let* ((default-directory (oldt-service-get-property "PATH"))
             (branch (oldt-project-get-property "BRANCH"))
             (current-branch (magit-get-current-branch))
             (source (oldt-project-get-property "SOURCE_BRANCH"))
             (staged (magit-staged-files))
             (unstaged (magit-unstaged-files))
             (untracked (magit-untracked-files)))
        (if (string= branch current-branch)
            (message "Already on branch %s" branch)
          (when (y-or-n-p (format "Switch to task branch %s (current %s%s%s%s)?" branch current-branch
                                  (if staged (format ", staged %d files" (length staged)) "")
                                  (if unstaged (format ", unstaged %d files" (length unstaged)) "")
                                  (if untracked (format ", untracked %d files" (length untracked)) "")))
            (magit-branch-or-checkout branch source)
            (magit-branch-checkout branch)))))))

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

(require 'request)
(require 'ts)

(defvar oldt-jira-login "" "Your Jira email address")
(defvar oldt-jira-api-token "" "Jira API token, see how to generate it here: https://confluence.atlassian.com/cloud/api-tokens-938839638.html")
(defvar oldt-jira-issue-url "https://flocktory.atlassian.net/rest/api/latest/issue/")

(defun oldt-jira-ticket-url (ticket &optional parameter)
  (concat
   oldt-jira-issue-url
   ticket
   (if parameter
       (concat "/" parameter)
     "")))

(defun oldt-jira-get-auth-token ()
  (concat "Basic " (base64-encode-string (concat oldt-jira-login ":" oldt-jira-api-token))))

(cl-defun oldt-jira-worklog-add (&key comment started time-spent)
  (interactive)
  (oldt-jira-post
   :data (json-encode '(("comment" . comment)
                        ("started" . started)  ;; "2020-02-20T09:23:19.552+0000"
                        ("timeSpent" . time-spent))) ;; "5m"
   :method "worklog"
   :success (cl-function (lambda (&key data &allow-other-keys) (pp data)))
   :error (cl-function (lambda (&key data &allow-other-keys) (pp data)))))

(cl-defun oldt-jira-worklog-show (&key data &allow-other-keys)
  (let-alist data
    (let ((outbuf (get-buffer-create "*jira-worklog*"))
          (time-format "[%Y-%m-%d %a %H:%M]"))
      (with-current-buffer outbuf
        (delete-region (point-min) (point-max))
        (org-mode)
        (goto-char (point-max))

        (cl-flet* ((iso (time-string)
                        (parse-iso8601-time-string time-string))

                   (iso-to-org (time-string)
                               (format-time-string time-format (iso time-string))))

          (cl-loop for wl across-ref .worklogs
                   do (let* ((comment (alist-get 'comment wl))
                             (created (iso-to-org (alist-get 'created wl)))
                             (updated (iso-to-org (alist-get 'updated wl)))
                             (started (iso-to-org (alist-get 'started wl)))
                             (time-spent-seconds (alist-get 'timeSpentSeconds wl))
                             (finished (->> wl
                                            (alist-get 'started)
                                            (iso-to-org)
                                            (ts-parse-org)
                                            (ts-adjust 'second time-spent-seconds)
                                            (ts-format time-format))))
                        (insert (format "* %s\n" (or comment "Mess")))
                        (insert ":LOGBOOK:\n")
                        (insert "CLOCK: " started "--" finished " => 0:00")
                        (org-clock-update-time-maybe)
                        (end-of-line)
                        (insert "\n")
                        (insert ":END:\n"))))

        (goto-char (point-min))
        (insert "#+BEGIN: clocktable :formula %\n#+END\n\n")
        (goto-char (point-min))
        (org-ctrl-c-ctrl-c))
      (switch-to-buffer-other-window outbuf)
      (org-show-all))))

(cl-defun oldt-jira-project-status-update (&key data &allow-other-keys)
  (save-window-excursion
    (save-excursion
      (let-alist data
        (message "Setting JIRA_TASK_STATUS property extracted from Jira task")
        (oldt-project-set-property "JIRA_TASK_STATUS" .fields.status.name)
        (let* ((jira-state (->> .fields.status.name
                                (s-replace " " "_" )
                                (upcase)))
               (org-state (cond
                           ((s-equals-p jira-state "IN_PROGRESS") "STARTED")
                           (t jira-state))))
          (oldt-project-set-property "TODO_STATE" org-state))))))

(cl-defun oldt-jira-ticket-capture (&key data &allow-other-keys)
 (save-window-excursion
   (save-excursion
     (let-alist data
       (let ((summary .fields.summary))
         (message "Going to last stored headline")
         (org-capture-goto-last-stored)
         (message "Setting ITEM property extracted from Jira task")
         (oldt-project-set-property "ITEM" (concat summary " [0%]")))))))

(cl-defun oldt-jira-get (&key ticket success error method sync)
  (when-let (ticket (oldt-project-get-property "TICKET"))
    (request (oldt-jira-ticket-url ticket method)
      :headers `(("Authorization" . ,(oldt-jira-get-auth-token)))
      :parser 'json-read
      :success success
      :error error
      :sync sync)))

(cl-defun oldt-jira-post (&key ticket data success error method)
  (when-let (ticket (or ticket (oldt-project-get-property "TICKET")))
    (request (oldt-jira-ticket-url ticket method)
      :type "POST"
      :data data
      :headers `(("Authorization" . ,(oldt-jira-get-auth-token))
                 ("Content-Type" . "application/json"))
      :parser 'json-read
      :success success
      :error error)))

(cl-defun oldt-jira-error (&key data error-thrown symbol-status &allow-other-keys)
  (pp symbol-status)
  (pp data)
  (pp error-thrown))

(defun oldt-jira-get-ticket-worklog ()
  (interactive)
  (when-let (ticket (oldt-project-get-property "TICKET"))
    (let ((url (oldt-jira-ticket-url ticket "worklog")))
      (request url
        :headers `(("Authorization" . ,(oldt-jira-get-auth-token)))
        :parser 'json-read
        :success #'oldt-jira-worklog-show
        :error #'oldt-jira-error
        :sync t))))

(defun oldt-jira-update-project-status ()
  (interactive)
  (oldt-jira-get
   :success #'oldt-jira-project-status-update))

(defun oldt-jira-capture-ticket-title (&optional ticket)
  (oldt-jira-get
   :success #'oldt-jira-ticket-capture))

(add-hook 'org-capture-before-finalize-hook 'oldt-jira-capture-ticket-title)

;; (request "https://flocktory.atlassian.net/rest/agile/latest/board/48/sprint?state=active"
;;            :headers `(("Authorization" . ,(oldt-jira-get-auth-token)))
;;            :parser 'json-read
;;            :success (cl-function (lambda (&key data &allow-other-keys)
;;                          (prin1 (mapcar (lambda (item) (cdr (assq 'id item)))
;;                                         (let-alist data .values))))))

;; (request
;;  (url-encode-url "https://flocktory.atlassian.net/rest/api/latest/search?fields=key&jql=sprint=207 AND assignee=dmitriy.akatov")
;;  :headers `(("Authorization" . ,(oldt-jira-get-auth-token)))
;;  :parser 'json-read
;;  :success (cl-function (lambda (&key data &allow-other-keys)
;;                          (prin1 (mapcar (lambda (item) (cdr (assq 'key item)))
;;                                         (let-alist data .issues))))))

(defun oldt-report-buffer-init ()
  (let ((todo-states "#+TODO: STARTED FAILED | OK"))
    (insert todo-states))
  (org-mode))

(defun oldt-report-create (headline)
  (let ((entry-id (org-id-uuid))
        (report-buffer (get-buffer-create "*oldt-projects-overview*")))
    (with-current-buffer report-buffer
      (if (string-empty-p (buffer-string))
          (oldt-report-buffer-init)
        (goto-char (point-max)))
      (org-insert-heading)
      (insert headline)
      (org-set-property "ID" entry-id)
      (org-set-property "VISIBILITY" "folded")
      (org-back-to-heading)
      (org-todo "STARTED"))
    entry-id))

(defun oldt-report-log (id state &rest messages)
  (let* ((report-buffer (get-buffer-create "*oldt-projects-overview*"))
         (note (cdr (assq 'note org-log-note-headings))))
    (setq note (org-replace-escapes
	        note
	        (list (cons "%u" (user-login-name))
		      (cons "%U" user-full-name)
		      (cons "%t" (format-time-string
			          (org-time-stamp-format 'long 'inactive)
			          org-log-note-effective-time))
		      (cons "%T" (format-time-string
			          (org-time-stamp-format 'long nil)
			          org-log-note-effective-time))
		      (cons "%d" (format-time-string
			          (org-time-stamp-format nil 'inactive)
			          org-log-note-effective-time))
		      (cons "%D" (format-time-string
			          (org-time-stamp-format nil nil)
			          org-log-note-effective-time))
		      (cons "%s" (cond
			          ((not org-log-note-state) "")
			          ((string-match-p org-ts-regexp
						   org-log-note-state)
				   (format "\"[%s]\""
					   (substring org-log-note-state 1 -1)))
			          (t (format "\"%s\"" org-log-note-state))))
		      (cons "%S"
			    (cond
			     ((not org-log-note-previous-state) "")
			     ((string-match-p org-ts-regexp
					      org-log-note-previous-state)
			      (format "\"[%s]\""
				      (substring
				       org-log-note-previous-state 1 -1)))
			     (t (format "\"%s\""
				        org-log-note-previous-state)))))))
    (with-current-buffer report-buffer
      (goto-char (point-max))
      (search-backward id)
      (org-back-to-heading)
      (when state
        (org-todo state))
      (goto-char (org-log-beginning t))
      ;; Make sure point is at the beginning of an empty line.
      (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert "\n")))
            ((looking-at "[ \t]*\\S-") (save-excursion (insert "\n"))))
      (if-let (itemp (org-in-item-p))
          (indent-line-to
             (let ((struct (save-excursion
                             (goto-char itemp) (org-list-struct))))
               (org-list-get-ind (org-list-get-top-point struct) struct)))
        (org-indent-line))
      (insert (org-list-bullet-string "-") note)
      (let ((ind (org-list-item-body-column (line-beginning-position))))
        (dolist (message messages)
          (insert "\n")
          (indent-line-to ind)
          (insert message)))
      (org-set-visibility-according-to-property))
    id))

(aio-defun oldt-process-report (project-name description log-id promise)
  (let* ((result (aio-await promise))
         (state (cond ((> result 0) "FAILED")
                      ((= result 0) "OK"))))
    (oldt-report-log log-id state description)))

(aio-defun oldt-git-project-overview (project-directory)
  (loop for .git in (directory-files-recursively project-directory "^.git$" t)
        do (oldt--directory-overview .git)))

(aio-defun oldt--directory-overview (dir)
  (let* ((process-buffer (generate-new-buffer-name "*project-status*"))
         (default-directory (file-name-directory dir))
         (project-name (format "[[file+emacs:%s][%s]] "
                               default-directory
                               (file-name-nondirectory
                                (directory-file-name
                                 default-directory))))
         (log-id (oldt-report-create project-name)))
    (aio-await
     (oldt-process-report project-name "update repository" log-id
                          (oldt-start-process-async "git-remote-update" process-buffer
                                                    "git" "remote" "update")))
    (aio-await
     (oldt-process-report project-name "git pull" log-id
                          (oldt-start-process-async "git-pull" process-buffer
                                                    "git" "pull")))))

(setq oldt-note-reader--current-marker nil)

(defun oldt-read-next-note (pom)
  (save-excursion
    (let ((eol (save-excursion
                 (org-goto-marker-or-bmk pom)
                 (re-search-forward org-clock-drawer-end-re)
                 (point-marker))))
    (when (< pom eol)
      (condition-case nil
          (save-excursion
            (org-goto-marker-or-bmk pom)
            (search-forward "Note taken on")
            (point-marker))
        (error nil))))))

(defun oldt-logbook-reader ()
  (interactive)
  (setq oldt-note-reader--current-marker
        (oldt-read-next-note (or oldt-note-reader--current-marker
                                 (save-excursion
                                   (org-back-to-heading)
                                   (re-search-forward ":LOGBOOK:")
                                   (point-marker))))))

(defun oldt-service-add-class-variables (service path vars)
  (dir-locals-set-class-variables service vars)
  (dir-locals-set-directory-class path service))

(defun oldt-project-workon--clojure ()
  "Run cider if it is a clojure service."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let* ((proj-dir (oldt-service-get-property "PATH"))
             (clojure-project-fn (concat proj-dir "/project.clj")))
        (when (file-exists-p clojure-project-fn)
          (find-file clojure-project-fn)
          (unless (condition-case nil
                      (cider-nrepl-eval-session)
                    (error nil))
            (call-interactively #'cider-jack-in)))))))

(defun oldt-send-current-to-remote ()
  (interactive)
  (when (and (boundp 'oldt-source-dir)
             (boundp 'oldt-target-dir))
    (let* ((source (buffer-file-name))
           (target (s-replace oldt-source-dir oldt-target-dir source)))
      (make-directory (file-name-directory target) t)
      (copy-file source target t))))

(add-hook 'after-save-hook 'oldt-send-current-to-remote)

(defun oldt-evaluate-blocks-current-heading ()
  (org-back-to-heading)
  (save-excursion
    (save-restriction
      (org-save-outline-visibility nil
        (narrow-to-region (org-entry-beginning-position) (org-entry-end-position))
        (loop while (condition-case-unless-debug nil (org-babel-next-src-block) (user-error nil))
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
