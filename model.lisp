
(in-package :cl-fm)
(defconstant COL-ID 0)
(defconstant COL-NAME 1)
(defconstant COL-SIZE 2)
(defconstant COL-DATE 3)
(defconstant COL-Q 4)
(defconstant COL-DIR 5)


;;------------------------------------------------------------------------------
;; custom routines - called by renderer to
;; render the columns...
;;

(defun custom-id (column renderer model iterator)
  "renders the id column, seq row number"
  (declare (ignore column model iterator))
  (setf (gtk-cell-renderer-text-background-gdk renderer)
	(make-gdk-color :red 65000 :green 0 :blue 0) ) )

(defun custom-name (column renderer model iterator)
  "renders the name of the file or directory/"
  (declare (ignore column))
  (let ((name (uiop:native-namestring (gtk-tree-model-get-value model iterator COL-NAME))))
    (setf (gtk-cell-renderer-text-text renderer) name)))


(defun custom-size (column renderer model iterator)
  "renders the file size.  -1 is unknown (permission issues, etc)"
  (declare (ignore column))
  (let ((size  (gtk-tree-model-get-value model iterator COL-SIZE))
	(q (gtk-tree-model-get-value model iterator COL-Q)))
    (setf (gtk-cell-renderer-text-background-gdk renderer) (q-color q)
	  (gtk-cell-renderer-text-text renderer)
	  (if (= size -1)
		 (format nil "Unknown")
		 (format nil "~:d" size)))))

(defun custom-date (column renderer model iterator)
  "rernder mod date in yr-mo-da format"
  (declare (ignore column))
  (let ((date (first (gtk-tree-model-get model iterator COL-DATE))))
    (setf (gtk-cell-renderer-text-text renderer)
	  (with-output-to-string (str) (print-date str date)))) )


(defun create-column (fb number title &key (custom nil) (align nil) (scale 0.75) (expand nil))
  "helper - create and return a single column with a text renderer"
  (let ((renderer (gtk-cell-renderer-text-new)))
    ;(setf (gtk-cell-renderer-text-editable renderer) t)
    (let ((column (gtk-tree-view-column-new-with-attributes title renderer "text" number)))
      (setf (gtk-cell-renderer-is-expander renderer) t)
      (setf (gtk-cell-renderer-text-scale-set renderer) t) ;allow text to scale
      (setf (gtk-cell-renderer-text-scale renderer) scale)   ;scale a little smaller
      (when align (setf (gtk-cell-renderer-xalign renderer) align)) ;align data within cell
      (when custom (gtk-tree-view-column-set-cell-data-func ;custom renderer data
		    column renderer custom))
      (gtk-tree-view-column-set-sort-column-id column number)
      (gtk-tree-view-column-set-reorderable column t)
      (when expand (gtk-tree-view-column-set-expand column t))
      (when (= number COL-NAME) ;special handling for name column for editing
	(setf (filebox-column-name fb) column
	      (filebox-renderer-name fb) renderer))
      column)))


(defun create-columns (fb)
  "create all columns"
  (list (create-column fb COL-ID "#" :align 1.0 :custom #'custom-id)
	(create-column fb COL-NAME "Filename" :custom #'custom-name :expand t)
	(create-column fb COL-SIZE "Size" :align 1.0 :custom #'custom-size)
	(create-column fb COL-DATE "Mod" :custom #'custom-date)
	(create-column fb COL-Q    "Q" )
	(create-column fb COL-DIR "DIR" :custom #'custom-id)))

(defun create-model ()
  (let ((model
	 (make-instance 'gtk-tree-store
			              ;;; ID        NAME       SIZE    DATE    Q      DIR
			:column-types '("guint" "gchararray" "gint64" "guint" "guint" "guint"))))
    (g-signal-connect model "row-deleted" #'on-row-deleted)
    (g-signal-connect model "row-inserted" #'on-row-inserted)
    (g-signal-connect model "row-changed" #'on-row-changed)
    model))

(defun model-append-initial-dir (store i file-name)
  "append an initial directory entry"
  (gtk-tree-store-set
   store
   (gtk-tree-store-append store nil) ;iter
   i          ;ID
   (concatenate 'string (car (last (pathname-directory file-name))) "/");;TODO: portability
   -1         ;SIZE
   0          ;DATE
   #xf        ;Q
   1))

(defun model-append-initial-file (store i file-name)
  (gtk-tree-store-set
   store (gtk-tree-store-append store nil)
   i          ;ID
   (file-namestring file-name ) ;NAME
   -1         ;SIZE
   0          ;DATE
   #xf        ;Q
   0))

(defun model-refill (store path &key (include-dirs t))
  "clear gtk store and reload store with data from filesystem"
  (gtk-tree-store-clear store)
  ;; First load directories, then files...
;  (format t "I: ~A~%" (uiop:subdirectories path))
  (let ((i 1))
    (and include-dirs
	 (loop for file-name in (uiop:subdirectories path); (cl-fad:list-directory path)
	    do
	      (model-append-initial-dir store i file-name)
	      (incf i)))
    
    (loop for file-name in (uiop:directory-files path); (cl-fad:list-directory path)
       do
	 (model-append-initial-file store i file-name)
	 (incf i))))

(defun model-postprocess (store directory)
  "across all files in model, update size, date and q"
  (gtk-tree-model-foreach
   store
   (lambda (model path iter)
     (declare (ignore path))
     (let* ((fname (merge-pathnames (gtk-tree-model-get-value model iter COL-NAME)
				    directory)) ;build full filepath
	    
	    ;; size may fail if permissions are no-good...
	    (size (handler-case
		      (with-open-file (in fname :element-type '(unsigned-byte 8))
			(file-length in))
		    (t () -1))) ;on error, size is nil
	    (date (file-write-date fname))
	    (q (q-get fname)))
       (unless q (setf q #XF))
       (if (or (< q 0) (> q 15)) (setf q #XF)) ;TODO: handle range check better !!!

       (gtk-tree-store-set-value model iter COL-SIZE size)
       (gtk-tree-store-set-value model iter COL-DATE date)
       (gtk-tree-store-set-value model iter COL-Q q))
     nil))) ;continue walk


(defun model-set-q (model path iterator directory value )
  "called to modify q in file and model"
  (declare (ignore path))
  (let ((pathname (merge-pathnames (gtk-tree-model-get-value model iterator COL-NAME)
				   directory )))
    (q-set value pathname))
  (gtk-tree-store-set-value model iterator COL-Q value)
;  (gtk-tree-model-row-changed model path iterator )
  )

(defun on-row-changed (model tp iter)
  (declare (ignore tp))
  ;(format t "~A ROW-CHANGED ~A~%" (get-universal-time) (first (gtk-tree-model-get model iter COL-ID)))
  )
(defun on-row-inserted (model tp iter)
  (declare (ignore tp))
  ;(format t "~A ROW-INSERTED ~A~%" (get-universal-time) (first (gtk-tree-model-get model iter COL-ID)))
  )
(defun on-row-deleted (model tp)
  (declare (ignore model tp))
  ;(format t "~A ROW-DELETED ~A~%" (get-universal-time) tp )
  )
(defun model-path->name (model tree-path)
  "get iter from path"
  (gtk-tree-model-get-value
   model
   (gtk-tree-model-get-iter model tree-path)
   COL-NAME))


