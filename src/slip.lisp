(defpackage :slip
  (:use :cl :slip.utils)
  (:export #:slip #:dofiles #:set-file-ext #:get-front #:with-page
	   #:*livereload* #:*livereload-port*))
(in-package :slip)

(defvar *files* (list))
(defvar *serve* nil)
(defvar *livereload* nil)
(defvar *livereload-port* 35729)

(defmacro slip ((&key (src "src/") (dest "dist/") (clean t)) &body body)
  `(progn
     (timing "Site built in ~f seconds~%"
       (setf *files* (get-files ,src))
       ,@body
       (write-files ,dest *files* :clean ,clean))
     (when *serve*
       (serve ,dest))))

;;; Entry point and arg processing

(defun main (argv)
  (setf *serve* (equal "serve" (nth 1 argv)))
  (use-package :slip)
  (use-package :slip.stages)
  (load "site.lisp"))
