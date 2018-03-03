(defpackage :slip.utils
  (:use :cl)
  (:export #:to-string #:echo #:trim-prefix #:trim-suffix #:make-keyword #:hash-table-plist #:timing
	   #:replace-ext #:ignored-dir-p #:ignored-file-p #:not-ignored-dir-p #:not-ignored-file-p
	   #:collect-files #:file-or-directory-exists-p #:with-ctrlc #:file-copy))
(in-package :slip.utils)

(defun to-string (thing)
  (format nil "~a" thing))

(defun echo (&rest args)
  (format t "~{~a ~}~%" args))

(defun trim-prefix (prefix s)
  (if (str:starts-with? prefix s)
      (str:substring (length prefix) t s)
    s))

(defun trim-suffix (suffix s)
  (if (str:ends-with? suffix s)
      (str:substring 0 (- (length suffix)) s)
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

(defun not-ignored-dir-p (dir)
  (not (ignored-dir-p dir)))

(defun not-ignored-file-p (file)
  (not (ignored-file-p file)))

(defun collect-files (dir &optional (filter-dir #'not-ignored-dir-p) (filter-file #'not-ignored-file-p))
  (let ((results (list)))
    (defun process-dir (subdir)
      (dolist (file (uiop:directory-files subdir))
	(when (uiop:call-function filter-file file)
	  (push file results))))
    (uiop:collect-sub*directories dir t filter-dir #'process-dir)
    results))

(defun file-or-directory-exists-p (file)
  (cond ((uiop:file-exists-p file)
	 (merge-pathnames file))
	((uiop:directory-exists-p file)
	 (merge-pathnames file))
	((uiop:directory-exists-p (str:concat file "/"))
	 (merge-pathnames (str:concat file "/")))))

(defmacro with-ctrlc (&body body)
  `(handler-case
       (progn ,@body)
     (#+sbcl sb-sys:interactive-interrupt
       #+ccl  ccl:interrupt-signal-condition
       #+clisp system::simple-interrupt-condition
       #+ecl ext:interactive-interrupt
       #+allegro excl:interrupt-signal
       ()
       (uiop:quit))))

(defun file-copy (source destination)
  (with-open-file (in source :direction :input :element-type '(unsigned-byte 8))
    (with-open-file (out destination :direction :output
			 :if-exists :overwrite
			 :if-does-not-exist :create
			 :element-type '(unsigned-byte 8))
      (uiop:copy-stream-to-stream in out :element-type '(unsigned-byte 8)))))
