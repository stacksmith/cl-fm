(in-package :cl-fm)
;; filebox - a widget containing a list of files
(defconstant GTK-KEY-F3 #XFFC0)
(defconstant GTK-KEY-F5 #XFFC2)
(defun print-date (stream date)
  "Given a universal time date, outputs to a stream."
  (if (and date (> date 0))
      (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
	  (decode-universal-time date)
	(declare (ignore sec min hr dow dst-p tz))
	(format stream "~4,'0d-~2,'0d-~2,'0d" yr mon day))
      (format stream "")))


(defconstant COL-ID 0)
(defconstant COL-NAME 1)
(defconstant COL-SIZE 2)
(defconstant COL-DATE 3)
(defconstant COL-Q 4)



#|(defun fentry-path (fentry directory)
  "convert fentry name into full path"
  (merge-pathnames directory (fentry-name fentry)))
|#

(defparameter *color-q*
  (vector  (make-gdk-color :red 35000 :green 00000 :blue 0) 
	   (make-gdk-color :red 30000 :green 00000 :blue 0) 
	   (make-gdk-color :red 25000 :green 00000 :blue 0) 
	   (make-gdk-color :red 20000 :green 05000 :blue 0) 
	   (make-gdk-color :red 15000 :green 10000 :blue 0) 
	   (make-gdk-color :red 10000 :green 15000 :blue 0) 
	   (make-gdk-color :red 05000 :green 20000 :blue 0) 
	   (make-gdk-color :red 00000 :green 25000 :blue 0) 
	   (make-gdk-color :red 00000 :green 30000 :blue 0) 
	   (make-gdk-color :red 00000 :green 35000 :blue 0)))

(defparameter *color-black* (make-gdk-color :red 0 :green 0 :blue 0) )
(defun q-color (q) ;TODO: range-check q
  (if (= q #XF) *color-black*
      (elt *color-q* q ))
  )
					;
;; TODO: perhaps optimize file access for length date and q?
;; TODO: gtk-list-store-set is buggy... See if it can be rewritten?
(defun data-postprocess (fb)
  (model-postprocess (filebox-store fb) (filebox-path fb))
)

(defstruct filebox widget store path)
   
   #|"secondary pass - get sizes etc"
   (loop for fe across (filebox-data fb) do
   (let
   ((path (fentry-path fe (filebox-path fb)))) ;build full filepath ;
   (unless (cl-fad:directory-exists-p path ) ;exlude directories
   (setf (fentry-size fe)  (with-open-file (in path) (file-length in))
   (fentry-mod fe) (file-write-date path)
   (fentry-q fe) (q-get path)
   (fentry-q fe) (q-get path))
					;(format t "[~A ~A ~A]~%" (fentry-q fe) (q-get path) path ) ; ; ;
   )))
   |#

   

(defun fb-refill (fb)
  "clear gtk store and reload store with data from filesystem"
  (model-refill (filebox-store fb) (filebox-path fb)  ) )
;;------------------------------------------------------------------------------
;; custom routines - called by renderer
;;
(defun custom-id (column renderer model iterator)
  "id column custom render data function"
;  (format t "~A ~%" 	  (gtk-tree-model-get model iterator 1 ))
  (setf (gtk-cell-renderer-text-background-gdk renderer)
	(make-gdk-color :red 65000 :green 0 :blue 0) ) )

(defun custom-name (column renderer model iterator)
  (declare (ignore column))
  (let* (;(id (1- (first (gtk-tree-model-get model iterator COL-ID))))
	 
					;(fentry (elt (filebox-data *fb*) id))
	 (q (first (gtk-tree-model-get model iterator COL-Q)) )
	 (col (if q (elt *color-q* q) *color-black*)))
    (setf (gtk-cell-renderer-text-foreground-gdk renderer) (q-color q))
    
#|    (setf (gtk-cell-renderer-text-background-gdk renderer)
	  (if (= 0 (fentry-size fentry))
	      (make-gdk-color :red 32000 :green 32000 :blue 32000)
	      (make-gdk-color :red 65000 :green 65000 :blue 65000)) )
)
|#))

(defun custom-size (column renderer model iterator)
  (let ((size (first (gtk-tree-model-get model iterator COL-SIZE))))
    (if (> size 0)
	(setf (gtk-cell-renderer-text-text renderer)
	      (with-output-to-string (str) (format str "~:d" size))))))
(defun custom-date (column renderer model iterator)
  (let ((date (first (gtk-tree-model-get model iterator  3))))
    (setf (gtk-cell-renderer-text-text renderer)
	  (with-output-to-string (str) (print-date str date))))
;  (format t "DATE")
  )



(defun foreach-selected-file (fb func)
  "func is (lambda (model path iterator).."
  (gtk-tree-selection-selected-foreach
   (gtk-tree-view-get-selection (filebox-widget fb))	;extract selection
   (lambda (model path iterator)
     (let ((pathname (merge-pathnames (filebox-path fb)
				      (gtk-tree-model-get-value model iterator COL-NAME))))
       (funcall func pathname)))
   ))



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
    ;(gtk-tree-view-column-set-visible (gtk-tree-view-get-column view COL-Q) nil)
    view))


(defun filebox-reload (fb)
  "reload all data"
  (fb-refill fb)
  (data-postprocess fb)
  ;(fb-refill fb)
  )
(defun create-filebox (path)
  (let ((fb (make-filebox :path path
			  :store (make-instance
				  'gtk-list-store
				                 ;; ID        NAME       SIZE    DATE    Q
				  :column-types '("guint" "gchararray" "gint64" "guint" "guint")))))
    (setf (filebox-widget fb)
	  (create-filebox-widget (filebox-store fb))) 
	  
   
    
    (g-signal-connect
     (filebox-widget fb) "key-press-event"
     (lambda (tv eventkey)
       (format t "PRESS: [~X ~A]~%" (gdk-event-key-keyval eventkey) (gdk-event-key-keyval eventkey))
       (let ((keyval (gdk-event-key-keyval eventkey)))
	 (cond
	   ((eql keyval GTK-KEY-F3)
	    (foreach-selected-file fb (lambda (filename) (format t "~A~%" filename))) )
	   ((eql keyval GTK-KEY-F5) (format t "OK~%")  (filebox-reload fb) )
	   ((and (>= keyval #x30)
		 (<= keyval #x39))
	    (format t "0~%")
	    (foreach-selected-file fb (lambda (filename)
					(q-set (- keyval #x30) filename)))
	    (filebox-reload fb) )
	   (t (format t "NOPE")))
	 )
       t ;do not propagate
       ))
    ;; Double-click
    (g-signal-connect
     (filebox-widget fb) "row-activated"
     (lambda (tv path column)
       (format t "~A ~A ~A  ~%" tv path column)
       (let* ((model (gtk-tree-view-get-model tv))
	      (iter (gtk-tree-model-get-iter model path))
	      ;(sel (gtk-tree-view-get-selection tv))
	      (path (merge-pathnames (filebox-path fb)
				     (gtk-tree-model-get-value model iter COL-NAME))))
					;(format t "ITER ~A ~%" path )
	 (xxx path)
	 ;TODO: figure out application
	 (external-program:start "vlc" (list path))
	 ;; selected
;	 (gtk-tree-selection-selected-foreach sel (lambda (mod path iter) (format t "MULT SEL ~A~%" (gtk-tree-model-get-value mod iter COL-ID))))
	 )))
     (filebox-reload fb)
    fb)
  )



;-----------------------
