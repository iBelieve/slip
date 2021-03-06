(defpackage #:slip-asd
  (:use :cl :asdf))

(in-package :slip-asd)

(defsystem slip
  :name "slip"
  :author "Michael Spencer <sonrisesoftware@gmail.com"
  :maintainer "Michael Spencer <sonrisesoftware@gmail.com"
  :license "GPL3.0+"
  :homepage "https://github.com/ibelieve/slip"
  :version "0.1"
  :depends-on (:str
               :uiop
	       :cl-yaml
	       :cl-ppcre
	       :markdown.cl
	       :spinneret
	       :clack
	       :wookie
	       :trivial-mimes
	       :cl-fsnotify
	       :unix-opts
	       :websocket-driver-server
	       :cl-json)
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
		 (:file "slip")
		 (:file "core")
		 (:file "serve")
		 (:file "watch")
		 (:file "livereload")
		 (:file "stages"))))
  :description "A static site generator in Common Lisp"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :build-operation "program-op"
  :build-pathname "slip"
  :entry-point "slip:main")
