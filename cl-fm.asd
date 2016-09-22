;;;; cl-fm.asd

(asdf:defsystem #:cl-fm
  :description "A extensible Lisp File Manager in a GTK window."
  :author "Stacksmith <fpgasm@apple2.x10.mx>"
  :license "MIT"
  :depends-on (#:cl-cffi-gtk      ;gui
	       #:gtk-emacs-like-input
               #:cl-ppcre         ;fancy searches
	       #:external-program ;execute external program
	       )
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "xattr")
	       (:file "model")
	       (:file "drag-and-drop")
	       (:file "filebox")
               (:file "cl-fm" )))

