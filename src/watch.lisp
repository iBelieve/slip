(in-package :slip)

(defun watch-dirs (dirs)
  (dolist (dir dirs)
    (uiop:collect-sub*directories (merge-pathnames dir) t #'not-ignored-dir-p
				  (lambda (dir)
				    (cl-fsnotify:add-watch dir)))))

(defun type-for-change (file events)
  (let ((exists (file-or-directory-exists-p file)))
    (cond ((and (find :create events) (find :delete events))
	   (if exists :modify nil))
	  ((find :create events)
	   (if exists
	       (if (uiop:directory-pathname-p file) :create nil)
	       :delete))
	  (t
	   (if exists :modify :delete)))))

(defun cleanup-changes (changes-list)
  (let ((changes-map (make-hash-table :test #'equal))
	(changes nil))
    (dolist (change changes-list)
	(setf (gethash (to-string (car change)) changes-map)
	      (append (gethash (to-string (car change)) changes-map)
		      (list (cdr change)))))
    (maphash (lambda (file events)
	       (let ((type (type-for-change file events)))
		 (when (and type (not-ignored-file-p file))
		   (push (cons file type) changes))))
	     changes-map)
    changes))

(defun change-requires-rebuild-p (change src)
  (and (or (equal :delete (cdr change))
	   (not (uiop:subpathp (merge-pathnames (car change)) src)))
       (not-ignored-file-p (car change))))

(defun process-changed-files (changes src rebuild)
  (let ((complete-rebuild nil))
    (dolist (change changes)
      (when (change-requires-rebuild-p change src)
	(setf complete-rebuild t)
	(return)))
    (if complete-rebuild
	(progn
	  (echo "Rebuilding all files...")
	  (setf *files* (get-files src)))
	(setf *files* (mapcar (lambda (file)
				(format t "Rebuilding file: ~a~%" file)
				(parse-file file src))
			      (mapcar #'car changes))))
    (when *files*
      (funcall rebuild))))
  
(defun watch-and-rebuild (src dirs rebuild)
  (cl-fsnotify:open-fsnotify)
  (watch-dirs dirs)
  (with-ctrlc 
      (let ((src (merge-pathnames src)))
	(loop
	   (process-changed-files
	    (cleanup-changes (cl-fsnotify:get-events)) src rebuild)
	   (sleep 0.1))))
  (cl-fsnotify:close-fsnotify))
