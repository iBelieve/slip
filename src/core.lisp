(in-package :slip)

(defun get-files (dir)
  (let ((dir (truename dir)))
    (mapcar (lambda (file) (parse-file file dir))
	    (collect-files dir)))) 

(defun write-files (dir files &key clean)
  (when clean
    (uiop:delete-directory-tree (pathname dir)
				:validate t
				:if-does-not-exist :ignore))
  (dolist (file files)
    (let ((dest (merge-pathnames (getf file :name) dir))
	  (contents (getf file :contents)))
      (ensure-directories-exist dest)
      (if contents
	  (str:to-file dest contents)
	  (file-copy (getf file :path) dest)))))

(defun load-file (file)
  (let* ((s (str::from-file (getf file :path)))
	 (pattern (ppcre:create-scanner "---(.*)---" :single-line-mode t)))
    (multiple-value-bind (match group) (ppcre:scan-to-strings pattern s)
      (if match
	  (progn (setf (getf file :frontmatter) (hash-table-plist (yaml:parse (str:trim (aref group 0)))))
		 (setf (getf file :contents) (str:trim (str:substring (length match) t s))))
	  (setf (getf file :contents) s)))))

(defun parse-file (path dir)
  (let ((filename (trim-prefix (to-string dir) (to-string path))))
    (list :name filename
	  :path path
	  :frontmatter nil
	  :contents nil)))

(defun set-file-ext (ext file)
  (setf (getf file :name) (replace-ext ext (getf file :name))))

(defun frontmatter (file key &optional default)
  (when (not (getf file :contents))
    (load-file file))
  (if (getf file :frontmatter)
      (getf (getf file :frontmatter) key default)
      default))

(defun contents (file)
  (when (not (getf file :contents))
    (load-file file))
  (getf file :contents))

(defmacro dofiles (file ext &body body)
  `(dolist (,file *files*)
     (when (equal ,ext (pathname-type (getf ,file :name)))
       (setf *path* (str:concat "/" (trim-suffix "index.html" (to-string (getf ,file :name)))))
       ,@body)))
 
(defmacro with-page ((&key title styles) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (dolist (stylesheet ,styles)
	 (:link :rel "stylesheet" :href stylesheet)))
      (:body
       ,@body
       (when *watch*
	 (:script (:raw *livereload-script*)))))))
