(in-package :cl-fm)
;; filebox - a widget containing a list of files

(defmacro fb-signal-connect (instance detailed-signal handler (&rest parameters))
  "like gtk-signal-connect, but
1) specifies the handler's parameters;
2) calls the handler with lexical fb in front of the parameters"
  `(g-signal-connect ,instance ,detailed-signal
		     (lambda (,@parameters) (,handler fb ,@parameters))))

(defun print-date (stream date)
  "Given a universal time date, outputs to a stream."
  (if (and date  (> date 0))
      (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
	  (decode-universal-time date)
	(declare (ignore sec min hr dow dst-p tz))
	(format stream "~4,'0d-~2,'0d-~2,'0d" yr mon day))))
 
  

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

(defstruct filebox widget store path window)
  
(defparameter *dragged-onto* nil) 

(defun foreach-selected-file (fb func)
  "func is (lambda (model path iterator).."
  (gtk-tree-selection-selected-foreach
   (gtk-tree-view-get-selection (filebox-widget fb))
   func))
   
 
(defun create-filebox-widget (model)
  "create gtk widget"
  (let ((view (make-instance 'gtk-tree-view
			     :model model))) 
    (loop for column in (create-columns) do
	 (gtk-tree-view-append-column view column))
    (gtk-tree-view-set-rules-hint view 1) ;display stripes
    (gtk-tree-selection-set-mode (gtk-tree-view-get-selection view) :multiple)
    ;invisible columns
    (gtk-tree-view-column-set-visible (gtk-tree-view-get-column view COL-ID) nil)
    (gtk-tree-view-column-set-visible (gtk-tree-view-get-column view COL-DIR) nil)
    (gtk-tree-view-column-set-visible (gtk-tree-view-get-column view COL-Q) nil)
    ;;
    (gtk-tree-view-enable-grid-lines view )
    (gtk-tree-view-set-reorderable view nil)
    (setf (gtk-widget-can-focus view) t)
    (setf (gtk-tree-view-enable-search view) nil); prevent key eating search box
    (drag-and-drop-setup view) ;see "drag-and-drop.lisp"
;    (keystroke-setup view) ;see "keystroke.lisp"

    (gdk-threads-add-idle #'(lambda ()
			   ;   (format t "IDLE..." )
			   ;   (sleep 10)
			   ;   (format t "IDLE...>~%" )
			      nil  ))
    view))

(defun filebox-reload (fb)
  "reload all data"
  (with-slots (store path) fb
    (model-refill store path  :include-dirs t)   
    (model-postprocess store path)))

(defun filebox-set-path (fb fpath)
  (with-slots (path window) fb
    (format t "FILEBOX-PATH: ~A~%PATH: ~A~%" path fpath)
    
    (setf path fpath
	  (gtk-window-title window) (concatenate 'string "cl-fm  " fpath))
    (filebox-reload fb)
    (format t "FILEBOX-PATH: DONE~%")))

(defun filebox-up (fb)
  (filebox-set-path
   fb
   (namestring (uiop:pathname-parent-directory-pathname (filebox-path fb)))))

(defun fb-selected-count (tv)
  "return t if multiple files selected"
  (let ((count 0))
    (gtk-tree-selection-selected-foreach
     (gtk-tree-view-get-selection tv)
     (lambda (model path iter)
       (declare (ignore model path iter))
       (incf count)))
    count))

(defun fb-pathname (fb name)
  "return the path to the named file in this fb"
  (concatenate
   'string
   (namestring (uiop:native-namestring
		(merge-pathnames name (filebox-path fb))))))

(defmacro fb-model-value (col)
  "get the value from the model, using lexical 'model' & 'iter'"
  `(gtk-tree-model-get-value model iter ,col))

(defun on-row-activated (fb tv path column) ;aka double-click
  (declare (ignore column))
  (format t "ROW ACTIVATED ~A  ~%" path);
  (let* ((model (gtk-tree-view-get-model tv))
	 (iter (gtk-tree-model-get-iter model path))
	 (fpath (fb-pathname fb (fb-model-value COL-NAME))))
    (when (= (fb-selected-count tv) 1)
      (if (= 1 (fb-model-value COL-DIR))
	  (filebox-set-path fb fpath)
	  (external-program:start "vlc" (list path)))))) ;TODO: dispatch on filetype
	    

(defun create-filebox (path window)
  (let ((fb (make-filebox :path nil
			  :store (create-model)
			  :window window)))
    (setf (filebox-widget fb)
	  (create-filebox-widget (filebox-store fb))) 

    (fb-signal-connect (filebox-widget fb) "row-activated" on-row-activated (tv path column))
    
    (filebox-set-path fb path)
    fb))

