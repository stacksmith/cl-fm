(in-package :cl-fm)
(defparameter *pix-docs* (gdk-pixbuf-new-from-file (namestring (asdf:system-relative-pathname
								'cl-fm "resources/icon-docs.png"))))
(defparameter *pix-bad*  (gdk-pixbuf-new-from-file (namestring (asdf:system-relative-pathname
								'cl-fm "resources/icon-bad.png"))))
(defparameter *dnd-target-src* (vector
				;;(gtk-target-entry-new "text/html" 0 112)
				(gtk-target-entry-new "text/uri-list" 0 115)
				;;(gtk-target-entry-new "GTK_TREE_MODEL_ROW" 0 110)
				;; (gtk-target-entry-new "TEXT" 0 113)
				;; (gtk-target-entry-new "STRING" 0 114)
				))
(defparameter *dnd-target-dest-int* (gtk-target-list-new ()))
(defparameter *dnd-target-dest* (vector
				;;(gtk-target-entry-new "text/html" 0 112)
				(gtk-target-entry-new "text/uri-list" 0 115)
				;;(gtk-target-entry-new "GTK_TREE_MODEL_ROW" 0 110)
				;; (gtk-target-entry-new "TEXT" 0 113)
				;; (gtk-target-entry-new "STRING" 0 114)
				))

