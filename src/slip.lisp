(defpackage slip
  (:use :cl :str)
  (:export #:slip #:markdown #:spinneret #:with-page))
(in-package #:slip)

;;; Helper functions

(defun to-string (thing)
  (format nil "~a" thing))

(defun echo (&rest args)
  (format t "~{~a ~}~%" args))

(defun trim-prefix (prefix s)
  (if (str:starts-with? prefix s)
      (str:substring (length prefix) t s)
    s))

(defun make-keyword (name)
  (values (intern (string-upcase name) :keyword)))

(defun hash-table-plist (table)
  (let ((plist nil))
    (maphash (lambda (k v)
               (setf plist (list* (make-keyword k) v plist)))
             table)
    plist))

;;; Filename functions

(defun replace-ext (ext path)
  (make-pathname :type ext :defaults path))

(defun is-ignored-dir (dir)
  (str:ends-with? "/.git/" (to-string dir)))

(defun is-ignored-file (file)
  (let ((filename (file-namestring file)))
    (or (str:ends-with? "~" filename)
	(str:starts-with? ".#" filename)
	(and (str:starts-with? "#" filename)
	     (str:ends-with? "#" filename)))))

(defun file-copy (source destination)
  (with-open-file (in source :direction :input)
    (with-open-file (out destination :direction :output)
      (uiop:copy-stream-to-stream in out))))

;;; Slip internal functions

(defvar *files* (list))

(defun collect-files (dir filter-dir filter-file)
  (let ((results (list)))
    (defun process-dir (subdir)
      (dolist (file (uiop:directory-files subdir))
	(when (uiop:call-function filter-file file)
	  (push file results))))
    (uiop:collect-sub*directories dir t filter-dir #'process-dir)
    results))
  
(defun get-files (dir)
  "Get a list of files to be processed inside dir (and relative to
dir), excluding certain ignored directories and files"
  (let ((dir (truename dir)))
    (mapcar (lambda (file) (parse-file file dir))
	    (collect-files dir (lambda (dir) (not (is-ignored-dir dir)))
			   (lambda (file) (not (is-ignored-file file)))))))

(defun write-files (dir files &key clean)
  (if clean
      (uiop:delete-directory-tree (pathname dir)
				  :validate t
				  :if-does-not-exist :ignore))
  (ensure-directories-exist dir)
  (dolist (file files)
    (let ((dest (merge-pathnames (getf file :name) dir))
	  (contents (getf file :contents)))
      (str:to-file dest contents))))

(defun parse-frontmatter (s)
  (let ((pattern (ppcre:create-scanner "---(.*)---" :single-line-mode t)))
    (multiple-value-bind (match group) (ppcre:scan-to-strings pattern s)
      (if match
	  (list :frontmatter (hash-table-plist (yaml:parse (str:trim (aref group 0))))
		:contents (str:trim (str:substring (length match) t s)))
	(list :frontmatter nil
	      :contents s)))))

(defun parse-file (path dir)
  (let ((filename (trim-prefix (to-string dir) (to-string path)))
	(file (parse-frontmatter (str::from-file path))))
    (list :name filename
	  :path path
	  :frontmatter (getf file :frontmatter)
	  :contents (getf file :contents))))

(defun set-file-ext (ext file)
  (setf (getf file :name) (replace-ext ext (getf file :name))))

(defun get-front (file key)
  (if (getf file :frontmatter)
      (getf (getf file :frontmatter) key)))

(defmacro dofiles (ext &body body)
  `(dolist (file *files*)
     (when (equal ,ext (pathname-type (getf file :name)))
       ,@body)))
 
(defmacro timing (msg &body body)
  (let ((start-time (gensym))
	(end-time (gensym))
	(result (gensym))
	(duration (gensym)))
    `(let* ((,start-time (get-internal-real-time))
	    (,result (progn ,@body))
	    (,end-time (get-internal-real-time))
	    (,duration (/ (- ,end-time ,start-time)
			  internal-time-units-per-second)))
       (format t ,msg ,duration)
       ,result)))

(defun load-layouts (dir)
  (dolist (file (collect-files dir t
			       (lambda (file) (equal "lisp" (pathname-type file)))))
    (load file)))

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))

;;; Slip plugins and main slip function

(defun markdown ()
  (dofiles "md"
    (let* ((markdown (getf file :contents))
	   (html (str:trim (markdown.cl:parse markdown))))
      (set-file-ext "html" file)
      (setf (getf file :contents) html))))

(defun spinneret (&key (layouts "layouts/") default)
  (load-layouts layouts)
  (dofiles "html"
    (let ((layout-name (or (get-front file :layout) default)))
      (when layout-name
	(let ((frontmatter (copy-list (getf file :frontmatter))))
	  (remf frontmatter :layout)
	  (let* ((layout (intern (string-upcase (str:concat "layout-" layout-name))))
		 (args `(:contents ,(getf file :contents) ,@frontmatter))
		 (contents (apply layout args)))
	    (setf (getf file :contents) contents)))))))

(defmacro slip ((&key (src "src/") (dest "dist/")) &body body)
  `(timing "Site built in ~f seconds"
     (setf *files* (get-files ,src))
     ,@body
     (write-files ,dest *files* :clean t)))

;;; Entry point and arg processing

(defun main (argv)
  (use-package :slip)
  (load "site.lisp"))
  
(defun test ()
  (intern "layout-blogpost"))
