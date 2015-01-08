;;; pman-project.el --- pman project layer
;;
;; Copyright (C) 2014  Matthias Pfeifer
;;
;; Author: Matthias Pfeifer <mpfeifer77@gmail.com>
;;
;; Keywords: software project manager emacs lisp
;;
;; object model
;;  projects are described by alists with following associations
;;   - name
;;   - project-root
;;   - jre
;;   - classpath

(defvar pman--projects '() "List of defined projects.")

(defvar pman--project nil "Current project.")

(defconst pman--name-p (string-to-symbol "pman--name"))
(defconst pman--classpath-p (string-to-symbol "pman--classpath"))
(defconst pman--jre-p (string-to-symbol "pman--jre"))
(defconst pman--project-root-p (string-to-symbol "pman--project-root"))

(defun pman--define-project (name tld jre &rest classpath)
  "Define a new project."
  (let ((project-descriptor (list name tld jre classpath)))
    (add-to-list 'pman--projects project-descriptor)))

;; Test
;; (setq pman--projects nil)
;; (pman--define-project "test1" "/tmp/" "/home/user/java/jre/jre7.1_70/" "~/java/lib/" "/home/user/java/jdk/jdk1.7.0_71/jre/lib/rt.jar")
;; (pman--define-project "EchoServer" "/home/user/java/EchoServer/" "/home/user/java/jre/jre7.1_70/jre/" "/home/user/java/jdk/jdk1.7.0_71/jre/lib/rt.jar")

(defsubst pman--project-name (project)
  "Extract project name from project descriptor PROJECT."
  (nth 0 project))
;; test: (pman--project-name (car pman--projects))

(defsubst pman--project-root (project)
  "Extract project root directory from project descriptor PROJECT."  
  (nth 1 project))
;; test: (pman--project-root (car pman--projects))

(defsubst pman--project-jre (project)
  "Extract project java runtime environment path from project descriptor PROJECT."  
  (nth 2 project))
;; test: (pman--project-jre (car pman--projects))

(defsubst pman--classpath (project)
  "Extract project classpath from project descriptor PROJECT."  
  (nth 3 project))
;; test: (pman--classpath (car pman--projects))

(defun pman--project-directories ()
  "Return a list of project root directories of currently defined projects."
  (mapcar #'(lambda (project)
	      "Extract project-root field from project descriptor PROJECT."
	      (pman--project-root project)) pman--projects))
;; test: (pman--project-directories)

(defun pman--project-for-cwd ()
  "Find project descriptor for current file. nil if none can be found."
  (let* ((projects (cdr pman--projects))
	(project (car pman--projects))
	(candidate nil)
	(cwd (file-name-directory buffer-file-name))
	(candidate-root-length -1))
    (while project
      (let* ((project-root (pman--project-root project)))
	(when (and
	       (string-prefix-p project-root cwd)
	       (or (not candidate)
		   (> (length project-root) candidate-root-length)))
	  (setq candidate project
		candidate-root-length (length (pman--project-root candidate)))))
      (setq project (car projects)
	    projects (cdr projects)))
    candidate))
;; test: (pman--project-for-cwd)

(provide 'pman-project)
