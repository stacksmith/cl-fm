(in-package :cl-fm)
;; filebox - a widget containing a list of files

(defun print-date (stream date)
  "Given a universal time date, outputs to a stream."
  (if (and date (> date 0))
      (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
	  (decode-universal-time date)
	(declare (ignore sec min hr dow dst-p tz))
	(format stream "~4,'0d-~2,'0d-~2,'0d" yr mon day))
      (format stream "")))

#|(defun fentry-path (fentry directory)
  "convert fentry name into full path"
  (merge-pathnames directory (fentry-name fentry)))
|#

(defparameter *color-q*
  (vector
   (gdk-color-parse "#FFFFFF") ;0 is uses as no q   
   (gdk-color-parse "#DDFFDB")
   (gdk-color-parse "#E5F3DA")
   (gdk-color-parse "#EEE8D9")
   (gdk-color-parse "#F6DCD8")
   (gdk-color-parse "#FFD1D8")
   
 ;;  (gdk-color-parse "#B2FFAD")
 ;;  (gdk-color-parse "#DCFFAB")
 ;;  (gdk-color-parse "#FFF6AA")
 ;;  (gdk-color-parse "#FFC9A9")
 ;;  (gdk-color-parse "#FFA8B4")
   
   ;;(gdk-color-parse "#A1FD9B")
;;	  (gdk-color-parse "#D3FD98")
;;	  (gdk-color-parse "#FDF296")
;;	  (gdk-color-parse "#FDBA93")
;;	  (gdk-color-parse "#FD91A1")


   (gdk-color-parse "#FFFFFF")
	  (gdk-color-parse "#FFFFFF")

	  (gdk-color-parse "#FFFFFF")
	  (gdk-color-parse "#FFFFFF")
	  (gdk-color-parse "#FFFFFF")
	  (gdk-color-parse "#FFFFFF")

	  (gdk-color-parse "#FFFFFF")
	  (gdk-color-parse "#FFFFFF")
	  (gdk-color-parse "#FFFFFF")
	  (gdk-color-parse "#FFFFFF")  ))
#|
(make-gdk-color :red 35000 :green 00000 :blue 0) 
(make-gdk-color :red 35000 :green 00000 :blue 0) 
(make-gdk-color :red 30000 :green 00000 :blue 0) 
(make-gdk-color :red 25000 :green 00000 :blue 0) 
(make-gdk-color :red 20000 :green 05000 :blue 0) 
(make-gdk-color :red 15000 :green 10000 :blue 0) 
(make-gdk-color :red 10000 :green 15000 :blue 0) 
(make-gdk-color :red 05000 :green 20000 :blue 0) 
(make-gdk-color :red 00000 :green 25000 :blue 0) 
(make-gdk-color :red 00000 :green 30000 :blue 0) 
(make-gdk-color :red 00000 :green 35000 :blue 0)
|#
(defparameter *color-black* (make-gdk-color :red 0 :green 0 :blue 0) )
(defparameter *color-white* (gdk-color-parse "#FFFFFF"))

(defun q-color (q) ;TODO: range-check q
  (if (= q #XF) *color-white*
      (elt *color-q*  q )))



(defstruct filebox widget store path window)
  
(defparameter *dragged-onto* nil) 
;;------------------------------------------------------------------------------
;; custom routines - called by renderer
;;
(defun custom-id (column renderer model iterator)
  "id column custom render data function"
  (declare (ignore column model iterator))
;  (format t "~A ~%" 	  (gtk-tree-model-get model iterator 1 ))
  (setf (gtk-cell-renderer-text-background-gdk renderer)
	(make-gdk-color :red 65000 :green 0 :blue 0) ) )

(defun custom-name (column renderer model iterator)
  (declare (ignore column          ))
  (let ((name (uiop:native-namestring (gtk-tree-model-get-value model iterator COL-NAME))))
    (setf (gtk-cell-renderer-text-text renderer) name)))

(defun custom-size (column renderer model iterator)
  (declare (ignore column))
  (let ((size  (gtk-tree-model-get-value model iterator COL-SIZE))
	(q (gtk-tree-model-get-value model iterator COL-Q)))
    (setf (gtk-cell-renderer-text-background-gdk renderer)
	  (q-color q))
    (if (> size 0)
	(setf (gtk-cell-renderer-text-text renderer)
	      (with-output-to-string (str) (format str "~:d" size))))))

(defun custom-date (column renderer model iterator)
  (declare (ignore column))
  (let ((date (first (gtk-tree-model-get model iterator COL-DATE))))
    (setf (gtk-cell-renderer-text-text renderer)
	  (with-output-to-string (str) (print-date str date))))
;  (format t "DATE")
  )


(defun foreach-selected-file (fb func)
  "func is (lambda (model path iterator).."
  (gtk-tree-selection-selected-foreach
   (gtk-tree-view-get-selection (filebox-widget fb))	;extract selection
   func)
 #| (lambda (model path iterator)
    (let ((pathname (merge-pathnames (filebox-path fb)
				     (gtk-tree-model-get-value model iterator COL-NAME)))))
    (funcall func model path iterator filepath ))
 |#
 )

(defun on-idle ()
  ())

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
;;(gtk-tree-view-column-set-visible (gtk-tree-view-get-column view COL-Q) nil)
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
  (setf (filebox-path fb) path
	(gtk-window-title (filebox-window fb)) (concatenate 'string "cl-fm  " path))
  (filebox-reload fb))

(defun filebox-up (fb)
  (filebox-set-path
   fb
   (namestring (uiop:pathname-parent-directory-pathname (filebox-path fb))))
)
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
	      (path (uiop:native-namestring (merge-pathnames (filebox-path fb)
							     (gtk-tree-model-get-value model iter COL-NAME)))))
	 (if (zerop  dir)
	     (external-program:start "vlc" (list path))
	     (progn
	       (format t "[~A]~%" path)
	       (filebox-set-path fb (concatenate 'string (namestring path) "/"))))
	 
					;	 (format t "ITER ~A ~%" path )
					;TODO: figure out application
	 
		 ;; selected
					;	 (gtk-tree-selection-selected-foreach sel (lambda (mod path iter) (format t "MULT SEL ~A~%" (gtk-tree-model-get-value mod iter COL-ID))))
	 )))
    (filebox-set-path fb path)
    (filebox-reload fb) ;initial load
    fb)
  )



					;-----------------------
  
