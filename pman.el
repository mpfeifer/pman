;;; pman.el --- Project manager for java projects

;; Author: Matthias 
;; Keywords: java software project manager pman

(defgroup java nil "All things related to java development [mp]." :group 'development)
(defgroup pman nil "All things related to java software project manager pman [mp]." :group 'java)

(defcustom maven-home "/root/java/apache-maven-3.2.5/" "Location of maven installation." :type '(string) :group 'pman)
(defcustom java-home "/root/java/jdk1.7.0_71/""Location of java sdk installation." :type '(string) :group 'pman)
(defcustom pman-project-repository "~/java/" "Default directory for newly created projects." :type '(string) :group 'pman)
(defcustom pman-group-id "de.mp" "Default group id for new projects." :type '(string) :group 'pman)
(defcustom pman-descriptor-directory "~/.emacs.d/projects/" "Directory where pman finds project descriptors" :group 'pman)

(require 'pman-project)
(require 'pman-classpath)
(require 'pman-build)
(require 'ac-pman)

(pman-load-project-definitions)

;; Body of this macro is executed each time pman mode is enabled.
(define-minor-mode pman-mode
  "Java software project manager."
  :lighter " pman"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c p b") 'pman-build)
	    (define-key map (kbd "C-c p r") 'pman-run)
	    (define-key map (kbd "C-c p n") 'pman-new-project)
	    (define-key map (kbd "C-c p l") 'pman-show-log-buffer)
	    map)
  (make-variable-buffer-local 'pman-project)  
  (pman-set-current-project)
  (add-to-list 'ac-sources 'ac-source-classpath)
  (pman-set-environment))

;;
(provide 'pman)
