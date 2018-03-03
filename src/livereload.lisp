(in-package :slip)

(defparameter *livereload-js* 
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "assets/livereload.js")))
(defparameter *livereload-port* 35729)
(defparameter *livereload-script* "document.write('<script src=\"http://' + (location.host || 'localhost').split(':')[0] + ':35729/livereload.js\"></' + 'script>')")

(defvar *livereload-notifiers* (list))

(defun livereload (dir)
  (clack:clackup
   (lambda (env)
     (cond ((equal "/livereload.js" (getf env :request-uri))
	    `(200 (:content-type "application/javascript") (,*livereload-js*)))
	   ((equal "/livereload" (getf env :request-uri))
	    (livereload-listen env))
	   (t
	    '(404 (:content-type "text/html") ("404 not found")))))
   :port *livereload-port*
   :server :wookie
   :silent t))

(defun livereload-listen (env)
  (let* ((ws (make-server env))
	 (notifier (as:make-notifier
		    (lambda ()
		      (dolist (file *files*)
			(livereload-file-changed ws file)))
		    :single-shot nil)))
    (push notifier *livereload-notifiers*)
    (on :error ws
	(lambda (error)
	  (format t "LiveReload error: ~S~%" error)))
    (on :close ws
	(lambda (&key code reason)
	  (setf *livereload-notifiers* (remove notifier *livereload-notifiers*))
	  (as:free-notifier notifier)))
    (lambda (responder)
      (declare (ignore responder))
      (start-connection ws)
      (send ws (json:encode-json-plist-to-string
		     '(:command "hello"
		       :protocols ("http://livereload.com/protocols/official-7")
		       :serverName "slip"))))))

(defun livereload-send-changes ()
  (dolist (notifier *livereload-notifiers*)
    (as:trigger-notifier notifier)))

(defun livereload-file-changed (ws file)
  (let* ((path (trim-suffix "index.html" (to-string (getf file :name))))
	 (event-json `(:command "reload" :path ,path))
	 (event (json:encode-json-plist-to-string event-json)))
    (send ws event)))

