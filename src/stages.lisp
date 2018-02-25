(defpackage :slip.stages
  (:use :cl :slip.utils :slip)
  (:export #:markdown #:spinneret))
(in-package :slip.stages)


;;; Markdown

(defun markdown ()
  (dofiles file "md"
    (let* ((markdown (getf file :contents))
	   (html (str:trim (markdown.cl:parse markdown))))
      (set-file-ext "html" file)
      (setf (getf file :contents) html))))

;;; Spinneret

(defun load-layouts (dir)
  (dolist (file (collect-files dir t
			       (lambda (file) (equal "lisp" (pathname-type file)))))
    (load file)))

(defun spinneret (&key (layouts "layouts/") default)
  (load-layouts layouts)
  (dofiles file "html"
    (let ((layout-name (or (get-front file :layout) default)))
      (when layout-name
	(let ((frontmatter (copy-list (getf file :frontmatter))))
	  (remf frontmatter :layout)
	  (let* ((layout (intern (string-upcase (str:concat "layout-" layout-name))))
		 (args `(:contents ,(getf file :contents) ,@frontmatter))
		 (contents (apply layout args)))
	    (setf (getf file :contents) contents)))))))


