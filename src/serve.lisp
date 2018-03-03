(in-package :slip)

(defun serve (dir &key (use-thread t) (port *serve-port*))
  (clack:clackup
   (lambda (env)
     (serve-path (getf env :path-info) dir))
   :port port
   :server :wookie
   :use-thread use-thread
   :silent t))

(defun directory-listing (uri dir)
  (let ((title (str:concat "Index of " uri)))
    (with-page (:title title)
      (:h1 title)
      (:ul (dolist (file (collect-files dir))
	     (let ((filename (to-string (file-namestring file))))
	       (:li (:a :href (str:concat uri filename) filename))))))))

(defun serve-path (uri dir)
  (let* ((relative-path (str:substring 1 t uri))
	 (file (merge-pathnames relative-path dir)))
     (if (uiop:directory-pathname-p file)
	 (if (uiop:directory-exists-p file)
	     (if (uiop:file-exists-p (merge-pathnames "index.html" file))
		 `(200 (:content-type "text/html")
		       (,(str:from-file (merge-pathnames "index.html" file))))
		 `(200 (:content-type "text/html") (,(directory-listing (getf env :request-uri) file))))
	     '(404 (:content-type "text/plain") ("Directory not found")))
	 (if (uiop:file-exists-p file)
	     (let ((content (str:from-file file))
		   (mime (mimes:mime uri)))
	       `(200 (:content-type ,mime) (,content)))
	     '(404 (:content-type "text/plain") ("File not found"))))))
