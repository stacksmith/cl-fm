(in-package :cl-fm)
;; filebox - a widget containing a list of files

(defun print-date (stream date)
  "Given a universal time date, outputs to a stream."
  (if (and date  (> date 0))
      (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
	  (decode-universal-time date)
	(declare (ignore sec min hr dow dst-p tz))
	(format stream "~4,'0d-~2,'0d-~2,'0d" yr mon day))))

#|(defun fentry-path (fentry directory)
  "convert fentry name into full path"
  (merge-pathnames directory (fentry-name fentry)))
|#

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
  (model-refill (filebox-store fb) (filebox-path fb) :include-dirs t  ) 
  (model-postprocess (filebox-store fb) (filebox-path fb)))

(defun filebox-set-path (fb path)
 ; (format t "FILEBOX-PATH: ~A~%PATH: ~A~%" (filebox-path fb) path)
  (setf (filebox-path fb) path
	(gtk-window-title (filebox-window fb)) (concatenate 'string "cl-fm  " path))
  (filebox-reload fb))

(defun filebox-up (fb)
  (filebox-set-path
   fb
   (namestring (uiop:pathname-parent-directory-pathname (filebox-path fb)))))
(defun create-filebox (path window)
  (let ((fb (make-filebox :path nil
			  :store (create-model)
			  :window window)))
    (setf (filebox-widget fb)
	  (create-filebox-widget (filebox-store fb))) 

					;    (model-init (filebox-store fb))
     ;; Double-click
    (g-signal-connect
     (filebox-widget fb) "row-activated"
     (lambda (tv path column )
       (format t "ROW ACTIVATED ~A ~A ~A  ~%" tv path column)
       (let* ((model (gtk-tree-view-get-model tv))
	      (iter (gtk-tree-model-get-iter model path))
					;(sel (gtk-tree-view-get-selection tv))
	      (dir (gtk-tree-model-get-value model iter COL-DIR))
	      (path (uiop:native-namestring (merge-pathnames (gtk-tree-model-get-value model iter COL-NAME)
							     (filebox-path fb)))))
	 (if (zerop dir)
	     (external-program:start "vlc" (list path))
	     (progn
	       (format t "[~A]~%" path)
	       (format t "FILEBOX-PATH: ~A~%" (filebox-path fb))
	       (format t "VALUE: ~A~%"  (gtk-tree-model-get-value model iter COL-NAME))
	       (filebox-set-path fb (concatenate 'string (namestring path))))))))
    (filebox-set-path fb path)
    (filebox-reload fb) ;initial load
    fb))

