
(in-package :cl-fm)
;; filebox - a widget containing a list of files


(defun print-date (stream date)
  "Given a universal time date, outputs to a stream."
  (if (and date  (> date 0))
      (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
	  (decode-universal-time date)
	(declare (ignore sec min hr dow dst-p tz))
	(format stream "~4,'0d-~2,'0d-~2,'0d" yr mon day))))
 
  

(defun fb-pathname (fb name)
  "return the path to the named file in this fb"
  (namestring (uiop:native-namestring
	       (merge-pathnames name (filebox-path fb)))))

(defmacro fb-model-value (col)
  "get the value from the model, using lexical 'model' & 'iter'"
  `(gtk-tree-model-get-value model iter ,col))


(defparameter *color-q*
  (make-array 16
	      :element-type 'GDK-COLOR
	      :initial-contents
	      (mapcar #'gdk-color-parse
		      '("#FFFFFF" "#DDFFDB" "#E6F3DA" "#EEE8D9"
			"#F6DCD8" "#FFD1D8" "#FFFFFF" "#FFFFFF"
			"#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF"
			"#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF"))))
  
(defparameter *color-black* (make-gdk-color :red 0 :green 0 :blue 0) )
(defparameter *color-white* (gdk-color-parse "#FFFFFF"))

(defun q-color (q) ;TODO: range-check q
  (if (= q #XF) *color-white*
      (elt *color-q*  q )))

(defstruct filebox widget store path window
	   column-name renderer-name ;for in-place editing of filenames
	   eli)
  
(defparameter *dragged-onto* nil) 


(defun filebox-reload (fb)
   ;; Refilling the model may take time, so we will set a wait cursor.  In order for
  ;; the cursor redraw to happen, we have to run the refill in idle mode
  (let ((gwin (gdk-screen-get-root-window (gdk-screen-get-default))))
    (flet ((refill-prim ()
	     (unwind-protect
		  (with-slots (store path) fb
		    (model-refill store path  :include-dirs t)   
		    (model-postprocess store path))
	       (gdk::gdk-window-set-cursor gwin (gdk-cursor-new :left-ptr)))))
      ;;
      (with-slots (path window) fb    
	(gdk::gdk-window-set-cursor gwin (gdk-cursor-new :watch))
	(setf (gtk-window-title window) (concatenate 'string "cl-fm  " path))
	;; low priority seems to be necessary for the cursor to change
	(g-idle-add #'refill-prim :priority glib:+g-priority-low+)))))


(defun filebox-set-path (fb fpath)
  "set a new path for this filebox and reload"
  (setf (filebox-path fb) fpath)
  (filebox-reload fb))

(defun filebox-up (fb)
  (filebox-set-path
   fb
   (namestring (uiop:pathname-parent-directory-pathname (filebox-path fb)))))


;;==============================================================================
(defun on-row-activated (fb tv path column) ;
  "aka double-click.  Attempt to open file"
  (declare (ignore column))
  (format t "ACTIVATED")
  (let* ((model (gtk-tree-view-get-model tv))
	 (iter (gtk-tree-model-get-iter model path))
	 (fpath (fb-pathname fb (fb-model-value COL-NAME))))
    (when (= (fb-selected-count fb) 1)
      (if (= 1 (fb-model-value COL-DIR))
	  (filebox-set-path fb fpath)
	  (external-program:start "vlc" (list fpath)))))) ;TODO: dispatch on filetype

;;==============================================================================
(defun create-filebox-widget (fb)
  "create gtk widget"
  (setf (filebox-widget fb) (make-instance 'gtk-tree-view  :model (filebox-store fb)))
  (with-slots (widget) fb
    (loop for column in (create-columns fb) do
	 (gtk-tree-view-append-column widget column))
    (gtk-tree-view-set-rules-hint widget 1) ;display stripes

    (gtk-tree-selection-set-mode (gtk-tree-view-get-selection widget) :multiple)
					;invisible columns

    (gtk-tree-view-column-set-visible (gtk-tree-view-get-column widget COL-ID) nil)
    (gtk-tree-view-column-set-visible (gtk-tree-view-get-column widget COL-DIR) nil)
    (gtk-tree-view-column-set-visible (gtk-tree-view-get-column widget COL-Q) nil)
    ;;

    (gtk-tree-view-enable-grid-lines widget )
    (gtk-tree-view-set-reorderable widget nil)
    (setf (gtk-widget-can-focus widget) t)
    (setf (gtk-tree-view-enable-search widget) nil); prevent key eating search box

        
    (gdk-threads-add-idle #'(lambda ()
					;   (format t "IDLE..." )
					;   (sleep 10)
					;   (format t "IDLE...>~%" )
			      nil  ))))


(defun create-filebox (path window)
  (let ((fb (make-filebox :path nil
			  :store (create-model)
			  :window window
			  )))
    (create-filebox-widget fb) 
    (drag-and-drop-setup fb)		;see "drag-and-drop.lisp"

    (fb-signal-connect (filebox-widget fb) "row-activated" on-row-activated (tv path column))
    
  
    (filebox-set-path fb path)

    (init-name-editing fb) ;see name-editing.lisp
     fb))

