(in-package :slip)

(defun get-files (dir)
  (let ((dir (truename dir)))
    (mapcar (lambda (file) (parse-file file dir))
	    (collect-files dir (lambda (dir) (not (ignored-dir-p dir)))
			   (lambda (file) (not (ignored-file-p file)))))))

(defun write-files (dir files &key clean)
  (when clean
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
  (when (getf file :frontmatter)
    (getf (getf file :frontmatter) key)))

(defmacro dofiles (file ext &body body)
  `(dolist (,file *files*)
     (when (equal ,ext (pathname-type (getf ,file :name)))
       ,@body)))
 
