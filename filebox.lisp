(in-package :cl-fm)
;; filebox - a widget containing a list of files
(defconstant GDK-KEY-F3 #XFFC0)
(defconstant GDK-KEY-F5 #XFFC2)
(defun print-date (stream date)
  "Given a universal time date, outputs to a stream."
  (if (and date (> date 0))
      (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
	  (decode-universal-time date)
	(declare (ignore sec min hr dow dst-p tz))
	(format stream "~4,'0d-~2,'0d-~2,'0d" yr mon day))
      (format stream "")))


;; Some data is stored in the gtk store...
(defconstant COL-ID 0)
(defconstant COL-NAME 1)
(defconstant COL-SIZE 2)
(defconstant COL-DATE 3)

;; Data stored in the model can be sorted on etc.  Other data is stored in fentry.
(defstruct fentry name size mod q)

(defun load-fentries (dir)
  (map 'vector
       #'(lambda (name)
	   (make-fentry :name
			(file-namestring (string-right-trim "/" (namestring name)))
			:size -1
			:mod 0))
       (cl-fad:list-directory dir)))

(defun fentry-path (fentry directory)
  "convert fentry name into full path"
  (merge-pathnames directory (fentry-name fentry)))


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
	  
(defun data-postprocess (fb)
  "secondary pass - get sizes etc"
  (loop for fe across (filebox-data fb) do
       (let ((path (fentry-path fe (filebox-path fb)))) ;build full filepath
	 (unless (cl-fad:directory-exists-p path ) ;exlude directories
	   (setf (fentry-size fe)  (with-open-file (in path) (file-length in))
		 (fentry-mod fe) (file-write-date path)
		 (fentry-q fe) (q-get path)
		 (fentry-q fe) (q-get path))
	   ;(format t "[~A ~A ~A]~%" (fentry-q fe) (q-get path) path )
	   ))))

(defstruct filebox widget store path data)

(defun fb-refill (fb)
  "reload the gtk store with data"
  (gtk-list-store-clear (filebox-store fb))
  (loop for i from 1 to 1000
     for fentry across (filebox-data fb) do
       (gtk-list-store-set
	(filebox-store fb)
	(gtk-list-store-append (filebox-store fb))
	i
	(fentry-name fentry)
	(fentry-size fentry)
	;(with-output-to-string (str) (format str "~:d" (fentry-size fentry)))
	(fentry-mod fentry)
;	(with-output-to-string (str) (print-date str (fentry-mod fentry)))
	))
 ; (format t "REFILL..")
  )
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
  (let* ((id (- (first (gtk-tree-model-get model iterator COL-ID)) 1))
	 (fentry (elt (filebox-data *fb*) id))
	 (q (fentry-q fentry))
	 (col (if q (elt *color-q* q) *color-black*)))
    (setf (gtk-cell-renderer-text-foreground-gdk renderer) col)
    
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

(defun create-filebox-column (number title &key (custom nil) (align nil))
  "helper - create a single column"
  (let* ((renderer (gtk-cell-renderer-text-new))
	 (column (gtk-tree-view-column-new-with-attributes title renderer "text" number)))
    (setf (gtk-cell-renderer-text-scale-set renderer) t)
    (setf (gtk-cell-renderer-text-scale renderer) 0.8)
    (when align (setf (gtk-cell-renderer-xalign renderer) align)) ;align data within cell
    (when custom (gtk-tree-view-column-set-cell-data-func ;custom renderer data
		  column renderer custom))
    (gtk-tree-view-column-set-sort-column-id column number)
    (gtk-tree-view-column-set-reorderable column t)
    column))
(defun create-filebox-columns ()
  ;; Create columns
  (list (create-filebox-column COL-ID "#" :align 1.0 :custom #'custom-id)
	(create-filebox-column COL-NAME "Filename" :custom #'custom-name)
	(create-filebox-column COL-SIZE "Size" :align 1.0 :custom #'custom-size)
	(create-filebox-column COL-DATE "Mod" :custom #'custom-date)))

(defun create-filebox-widget (model)
  "create gtk widget"
  (let ((view (make-instance 'gtk-tree-view
			     :model model))) 
    (loop for column in (create-filebox-columns) do
	 (gtk-tree-view-append-column view column))
    (gtk-tree-view-set-rules-hint view 1) ;display stripes
    (gtk-tree-selection-set-mode (gtk-tree-view-get-selection view) :multiple)
    ;invisible id column
    (gtk-tree-view-column-set-visible (gtk-tree-view-get-column view 0) nil)
    view))


(defun filebox-reload (fb)
  "reload all data"
  (setf (filebox-data fb) (load-fentries (filebox-path fb)))
  (fb-refill fb)
  (data-postprocess fb)
  (fb-refill fb)
  )
(defun create-filebox (path)
  (let ((fb (make-filebox :path path
			  :store (make-instance
				  'gtk-list-store
				  :column-types '("guint" "gchararray" "gint64" "guint")))))
    (setf (filebox-widget fb)
	  (create-filebox-widget (filebox-store fb))) 
	  
   
    
    (g-signal-connect
     (filebox-widget fb) "key-press-event"
     (lambda (tv eventkey)
       (format t "PRESS: [~X ~A]~%" (gdk-event-key-keyval eventkey) (gdk-event-key-keyval eventkey))
       (case (gdk-event-key-keyval eventkey)
	 (#XFFC2 (format t "OK~%")  (filebox-reload fb) );
	 (otherwise (format t "NOPE"))
	 )
       t))
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
