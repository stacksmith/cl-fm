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
	       (:file "cl-cffi-gtk-fixes")
	       (:file "util")
	       (:file "fb-util")
	       (:file "xattr")
	       (:file "file-actions")
	       (:file "view")
	       (:file "model")
	       (:file "drag-and-drop")
	       (:file "selection")
	       (:file "name-editing")
	       (:file "filebox")
               (:file "cl-fm" )))

