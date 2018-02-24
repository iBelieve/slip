(defpackage :slip.utils
  (:use :cl)
  (:export #:to-string #:echo #:trim-prefix #:make-keyword #:hash-table-plist
	   #:timing #:replace-ext #:ignored-dir-p #:ignored-file-p #:collect-files))
(in-package :slip.utils)


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

(defun replace-ext (ext path)
  (make-pathname :type ext :defaults path))

(defun ignored-dir-p (dir)
  (str:ends-with? "/.git/" (to-string dir)))

(defun ignored-file-p (file)
  (let ((filename (file-namestring file)))
    (or (str:ends-with? "~" filename)
	(str:starts-with? ".#" filename)
	(and (str:starts-with? "#" filename)
	     (str:ends-with? "#" filename)))))

(defun collect-files (dir filter-dir filter-file)
  (let ((results (list)))
    (defun process-dir (subdir)
      (dolist (file (uiop:directory-files subdir))
	(when (uiop:call-function filter-file file)
	  (push file results))))
    (uiop:collect-sub*directories dir t filter-dir #'process-dir)
    results))
