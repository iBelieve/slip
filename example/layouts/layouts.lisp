(defun layout-default (&key title contents)
  (with-page (:title title)
    (:header
     (:h1 title))
    (:raw contents)))

(defun layout-post (&key title contents)
  (with-page (:title title)
    (:header
     (:h1 title))
    (:article (:raw contents))))
