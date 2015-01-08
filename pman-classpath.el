;;; pman-classpath.el --- pman classpath handling
;;
;; Copyright (C) 2014  Matthias Pfeifer
;;
;; Author: Matthias Pfeifer <mpfeifer77@gmail.com>
;; Keywords: java classpath software project manager

(defun pman--jarfile-to-classes-list (jarfile)
  "Take location of a jar file and extract classes from the file."
  (let ((classes nil))
    (with-temp-buffer "*classpath*"
		      (call-process "/usr/bin/zipinfo" nil t nil "-1" jarfile)
		      (goto-char (point-min))
		      (delete-matching-lines "[$]+" (point-min) (point-max))
		      (goto-char (point-min))
		      (while (re-search-forward "^.*\\.class$" (point-max) t)
			(setq classes
			      (cons
			       (replace-regexp-in-string
				"\\.class$" ""
				(replace-regexp-in-string "/" "." (match-string 0)))
				classes)))
		      classes)))
;; Test: (pman--jarfile-to-classes-list "/home/user/java/jdk/jdk1.7.0_71/lib/jconsole.jar")

(defun pman--expand-classpath (project)
  "Generates list of classes found in classpath of project PROJECT."
  (let ((classpath (pman--classpath project))
	(classes nil))
    (dolist (item classpath)
      (cond
       ((string-match "^.*\\.jar$" item) ;; handle *.jar entries in classpath
	(setq classes (append classes (pman--jarfile-to-classes-list item))))))
    classes))
;; Test: (pman--expand-classpath (car pman--projects))

(provide 'pman-classpath)
