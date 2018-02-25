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

(defun get-front (file key &optional default)
  (when (getf file :frontmatter)
    (getf (getf file :frontmatter) key default)))

(defmacro dofiles (file ext &body body)
  `(dolist (,file *files*)
     (when (equal ,ext (pathname-type (getf ,file :name)))
       ,@body)))
 
(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body
       ,@body
       (when *livereload*
	 (:script :src *livereload-port*))))))
