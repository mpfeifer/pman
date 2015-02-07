;;; pman-project.el --- pman project layer
;;
;; Author: Matthias 
;;
;; Keywords: software project manager emacs lisp
;;
;; object model
;;  projects are described by lists with following properties
;;   - name
;;   - project-root
;;   - jre
;;   - classpath

(defvar pman-projects '() "List of defined projects.")

(defvar pman-project nil "Current project project-descriptor.")

(defun pman-define-project (name tld jre classpath)
  "Define a new project."
  (let ((project-descriptor (list name tld jre classpath)))
    (add-to-list 'pman-projects project-descriptor)))

;;
;; load project definitions
;;
(pman-define-project "SimpleShell"
		     "/root/java/swt/"
		     nil
		     "/root/.m2/repository/org/eclipse/swt/org/eclipse/swt/gtk/linux/x86/4/3/swt/org.eclipse.swt.gtk.linux.x86/4.3/org.eclipse.swt.gtk.linux.x86-4.3.jar")

;; Test
;; (setq pman-projects nil)
;; (pman-define-project "test1" "/tmp/" "/home/user/java/jre/jre7.1_70/" "~/java/lib/" "/home/user/java/jdk/jdk1.7.0_71/jre/lib/rt.jar")
;; (pman-define-project "EchoServer" "/home/user/java/EchoServer/" "/home/user/java/jre/jre7.1_70/jre/" "/home/user/java/jdk/jdk1.7.0_71/jre/lib/rt.jar")

(defsubst pman-project-name (project)
  "Extract project name from project descriptor PROJECT."
  (nth 0 project))
;; test: (pman-project-name (car pman-projects))

(defsubst pman-project-root (project)
  "Extract project root directory from project descriptor PROJECT."  
  (nth 1 project))
;; test: (pman-project-root (car pman-projects))

(defsubst pman-project-jre (project)
  "Extract project java runtime environment path from project descriptor PROJECT."
  (let ((jre nil))
    (when project
      (if (nth 2 project)
	  (setq jre (nth 2 project))
	(setq jre java-home)))
    jre))
;; test: (pman-project-jre (car pman-projects))

(defsubst pman-classpath (project)
  "Extract project classpath from project descriptor PROJECT."  
  (nth 3 project))
;; test: (pman-classpath (car pman-projects))

(defun pman-set-current-project ()
  "Look in pman-projects if current project belongs to any of the defined projects.
Then set pman-project according."
  (interactive)
  (let ((project (pman-project-for-cwd)))
    (setq pman-project project)
    project))

(defun pman-project-directories ()
  "Return a list of project root directories of currently defined projects."
  (mapcar #'(lambda (project)
	      "Extract project-root field from project descriptor PROJECT."
	      (pman-project-root project)) pman-projects))
;; test: (pman-project-directories)

(defun pman-project-for-cwd ()
  "Find project descriptor for current file. Return nil if none can be found."
  (let* ((projects (cdr pman-projects))
	(project (car pman-projects))
	(candidate nil)
	(cwd (file-name-directory buffer-file-name))
	(candidate-root-length -1))
    (while project
      (let* ((project-root (pman-project-root project)))
	(when (and
	       (string-prefix-p project-root cwd)
	       (or (not candidate)
		   (> (length project-root) candidate-root-length)))
	  (setq candidate project
		candidate-root-length (length (pman-project-root candidate)))))
      (setq project (car projects)
	    projects (cdr projects)))
    candidate))

(defun pman-define-project-from-json (json)
  "Extract project definition from JSON. JSON should contain fields

name - the project name
root - the project root directory
jre  - the project classpath"
  (let* ((project-name (cdr (assoc 'name json)))
	 (project-root (cdr (assoc 'root json)))
	 (project-jre  (cdr (assoc 'jre json)))
	 (vector-classpath (cdr (assoc 'classpath json)))
	 (project-classpath nil)
	 (len (length vector-classpath))
	 (i 0))
  (while (< i len)
    (add-to-list 'project-classpath (elt vector-classpath i) t)
    (setq i (+ 1 i)))
  (pman-define-project project-name project-root project-jre project-classpath)))

(defun pman-load-project-definitions ()
  "Read project definitions from pman-descriptor-directory."
  (let ((files (directory-files "/tmp/projects/" t "^.*js$")))
    (while files
      (let ((file (car files)))
	(setq files (cdr files))
	(pman-define-project-from-json (json-read-file file))))))

;; test: (pman-project-for-cwd)

(provide 'pman-project)
