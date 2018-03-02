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

;;; Spinneret (layouts in Lisp)

(defun load-layouts (dir)
  (push dir *watch-dirs*)
  (dolist (file (collect-files dir t
			       (lambda (file) (equal "lisp" (pathname-type file)))))
    (load file)))

(defun spinneret (&key (layouts "layouts/") default)
  (load-layouts layouts)
  (dofiles file "html"
    (let ((layout-name (or (get-front file :layout) default)))
      (when layout-name
	(remf (getf file :frontmatter) :layout)
	(let* ((layout (intern (string-upcase (str:concat "layout-" layout-name))))
	       (args `(:contents ,(getf file :contents) ,@(getf file :frontmatter)))
	       (contents (apply layout args)))
	  (setf (getf file :contents) contents))))))

;;; Permalinks

(defun convert-to-index (filename)
  (let* ((base-dirname (or (pathname-directory filename) '(:relative)))
	 (dirname (append base-dirname (list (pathname-name filename)))))
    (make-pathname :directory dirname :name "index" :type "html")))

(defun permalinkp (file)
  (and (get-front file :permalink t)
       (not (equal "index" (pathname-name (getf file :name))))))

(defun permalinks ()
  (dofiles file "html"
    (when (permalinkp file)
      (setf (getf file :name) (convert-to-index (getf file :name))))
    (remf (getf file :frontmatter) :permalink)))
