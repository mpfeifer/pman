;;; pman.el --- Project manager for java projects

;; Copyright (C) 2014  Matthias Pfeifer

;; Author: Matthias Pfeifer <mpfeifer77@gmail.com>
;; Keywords: java

;; find-file-hook
;; - if file belongs to project enable pman minor mode
;; - clarify to which project file belongs
;; - define buffer local variable that keeps project descriptor
;; - calculate classes from classpath
;; - helm-browse classpath
;; - ac-classpath
;;
;; TODO
;; - do some caching

(require 'pman-project)
(require 'pman-classpath)

(define-minor-mode pman-mode
  "Java software project manager."
  :lighter " pman"
  :keymap (let ((map (make-sparse-keymap)))
	    map)
  (make-variable-buffer-local
   (defvar pman--project nil
     "Project descriptor for current project."))
  (setq pman--project (pman--project-for-cwd)))

;; To use pman's auto-complete enabled import helper for java
(add-hook 'java-mode-hook '(lambda ()
			     "Enable auto-completion for import statements."
			     (interactive)
			     (require 'ac-pman)
			     (add-to-list 'ac-sources 'ac-source-classpath)))
;;
(provide 'pman)
