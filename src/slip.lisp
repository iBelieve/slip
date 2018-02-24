(defpackage :slip
  (:use :cl :slip.utils)
  (:export #:slip #:dofiles #:set-file-ext #:get-front))
(in-package :slip)

(defvar *files* (list))

(defmacro slip ((&key (src "src/") (dest "dist/")) &body body)
  `(timing "Site built in ~f seconds~%"
     (setf *files* (get-files ,src))
     ,@body
     (write-files ,dest *files* :clean t)))

;;; Entry point and arg processing

(defun main (argv)
  (use-package :slip)
  (use-package :slip.stages)
  (load "site.lisp"))
