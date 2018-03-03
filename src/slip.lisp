(defpackage :slip
  (:use :cl :slip.utils :websocket-driver :wookie :wookie-plugin-export)
  (:export #:main #:slip #:dofiles #:set-file-ext #:get-front #:with-page
	   #:*livereload* #:*livereload-port* #:*path* #:*watch-dirs*))
(in-package :slip)

(defparameter *serve-port* 5000)

(defvar *serve* nil)
(defvar *watch* nil)
(defvar *watch-dirs* (list))
(defvar *files* (list))
(defvar *path* nil)

(defmacro slip ((&key (src "src/") (dest "dist/") (clean t)) &body body)
  `(progn
     (timing "Site built in ~f seconds~%"
       (setf *files* (get-files ,src))
       (setf *watch-dirs* (list ,src))
       ,@body
       (write-files ,dest *files* :clean ,clean))
     (when *watch*
       (echo "Watching for changes and serving on localhost:5000...")
       (livereload ,dest)
       (serve ,dest)
       (watch-and-rebuild ,src *watch-dirs*
			  (lambda ()
			    ,@body
			    (write-files ,dest *files* :clean nil)
			    (livereload-send-changes))))
     (when *serve*
       (echo "Serving site on localhost:5000...")
       (serve ,dest :use-thread nil))))

;;; Entry point and arg processing

(opts:define-opts
    (:name :help
           :description "print this help text"
           :short #\h
           :long "help"))

(defun main ()
  (use-package :slip)
  (use-package :slip.stages)
  (use-package :spinneret)

  (multiple-value-bind (options free-args) (opts:get-opts)
    (setf *serve* (equal "serve" (first free-args)))
    (setf *watch* (equal "watch" (first free-args)))
    (load "site.lisp")))
