;;; pman-build.el --- pman project layer
;;
;; Author: Matthias 
;;
;; Keywords: build java software project pman
;;

(defun pman-set-environment ()
  "Set pman specific environemnt variables (JAVA_HOME & co)."
  (setenv "MAVEN_HOME" maven-home)
  (setenv "JAVA_HOME" java-home)
  (setenv "PATH" (concat maven-home "/bin/" ":" java-home "/bin/" ":" (getenv "PATH"))))

(defsubst pman-maven-bin ()
  "Convenience defsubst to calculate the directory where maven binary can be found."
  (concat maven-home "/bin/mvn"))

(defun pman-show-log-buffer ()
  "Substitute current window buffer by pmans log buffer."
  (interactive)
  (switch-to-buffer "*pman*" nil t)
  (goto-char (point-max))
  (read-only-mode 1))

(defun pman-input-footer ()
  "Draw a nice seperator for the output of two runs from maven."
  (read-only-mode -1)
  (goto-char (point-max))
  (open-line 1)
  (forward-line 1)
  (open-line 1)
  (forward-line 1)
  (open-line 1)
  (forward-line 1)
  (insert "======================================================================================")
  (open-line 1)
  (forward-line 1)
  (read-only-mode 1))

(setq pman-group-id-history '())

(defun pman-mvn (working-directory-param &rest mvnargs)
  "Build current project."
  (interactive)
  (let ((working-directory nil)
	(maven-form '(call-process (pman-maven-bin) nil "*pman*" t)))
    (if working-directory-param
	(setq working-directory working-directory-param)
      (setq working-directory (pman-project-root pman-project)))
    (pman-show-log-buffer)
    (while mvnargs
      (setq arg (car mvnargs)
	    mvnargs (cdr mvnargs))
      (add-to-list 'maven-form arg t))
    (let ((default-directory working-directory))
	    (eval maven-form)
	    (pman-input-footer))))

(defun pman-build ()
  "Build current project."
  (interactive)
  (pman-mvn nil "package"))

(defun pman-run ()
  "Run current project."
  (interactive)
  (pman-mvn nil "exec:java"))

(defvar pman-group-id-history nil
  "History of group-ids of newly created java projects.")

(defun pman-new-project (project-name &optional group-id)
  "Build a new java project via mvn archetype."
  (interactive "sproject-name? ")
  (unless group-id
    (setq group-id (read-string "Group ID? " pman-group-id pman-group-id-history nil)))
  (unless (file-exists-p pman-project-repository)
    (make-directory pman-project-repository t))
  (pman-mvn pman-project-repository
	    "archetype:create"
	    "-DarchetypeGroupId=org.apache.maven.archetypes"
	    (concat "-DgroupId=pman.group.id" pman-group-id)
	    (concat "-DartifactId=" project-name)))

(provide 'pman-build)
