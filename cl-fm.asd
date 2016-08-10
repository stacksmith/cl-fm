;;;; cl-fm.asd

(asdf:defsystem #:cl-fm
  :description "A extensible Lisp File Manager in a GTK window."
  :author "Stacksmith <fpgasm@apple2.x10.mx>"
  :license "MIT"
  :depends-on (#:cl-cffi-gtk
               #:cl-fad
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "cl-fm")
	       (:file "util")))

