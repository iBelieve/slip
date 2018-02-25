(defpackage :slip.stages
  (:use :cl :slip.utils :slip)
  (:export #:markdown #:spinneret #:permalinks))
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

;;; Permalinks

(defun permalinks ()
  (dofiles file "html"
    (when (and (get-front file :permalink t)
	       (not (equal "index" (pathname-name (getf file :name)))))
      (let* ((filename (getf file :name))
	     (dirname (append (or (pathname-directory filename) '(:relative))
			      (list (pathname-name filename)))))
	(setf (getf file :name) (make-pathname :directory dirname
					       :name "index"
					       :type "html"))))
    (remf (getf file :frontmatter) :permalink)))
