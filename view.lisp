(in-package :cl-fm)

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
  (let ((name (uiop:native-namestring (gtk-tree-model-get-value model iterator COL-NAME)))
	(q (gtk-tree-model-get-value model iterator COL-Q)))
    (setf (gtk-cell-renderer-text-background-gdk renderer) (q-color q)
	  (gtk-cell-renderer-text-text renderer)
	  name)))


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
      (gtk-tree-view-column-set-sizing column :autosize)
      (gtk-tree-view-column-set-sort-column-id column number)
      (gtk-tree-view-column-set-reorderable column t)
      (when expand (gtk-tree-view-column-set-expand column expand))
      (when (= number COL-NAME) ;special handling for name column for editing
	(setf (filebox-column-name fb) column
	      (filebox-renderer-name fb) renderer))
      column)))

(defun create-column-name (fb number title &key (custom nil) (align nil) (scale 0.9) (expand nil))
  "helper - create the name column"
  (let ((column (gtk-tree-view-column-new)))
    (gtk-tree-view-column-set-title column title)
    (let ((renderer-icon (gtk-cell-renderer-pixbuf-new)))
      (gtk-tree-view-column-pack-start column renderer-icon :expand nil)
      (gtk-tree-view-column-set-attributes column renderer-icon "pixbuf" COL-ICON))
    (let ((renderer (gtk-cell-renderer-text-new)))
      (setf (gtk-cell-renderer-text-scale-set renderer) t) ;allow text to scale
      (setf (gtk-cell-renderer-text-scale renderer) scale)   ;scale a little smaller
      (when align (setf (gtk-cell-renderer-xalign renderer) align)) ;align data within cell
      (when custom (gtk-tree-view-column-set-cell-data-func ;custom renderer data
		    column renderer custom))
      (gtk-tree-view-column-set-attributes column renderer "text" COL-NAME)
      (gtk-tree-view-column-set-sizing column :autosize)
      (gtk-tree-view-column-set-reorderable column t)
      (gtk-tree-view-column-set-resizable column t)
      (gtk-tree-view-column-set-sort-column-id column number)
      ;;special handling for name column for editing
      (setf (filebox-column-name fb) column
	    (filebox-renderer-name fb) renderer)
      (gtk-tree-view-column-pack-start column renderer :expand expand))
    column))
  


(defun create-column-icon (fb number title &key (custom nil) (align nil) (scale 0.75) )
  "helper - create and return a single column with a text renderer"
  (let ((renderer (gtk-cell-renderer-pixbuf-new)))
    ;(setf (gtk-cell-renderer-text-editable renderer) t)
    (let ((column (gtk-tree-view-column-new-with-attributes title renderer "pixbuf" COL-ICON  )))
      column)))

(defun create-columns (fb)
  "return a list of newly-created columns"
  (list (create-column fb COL-ID "#" :align 1.0 :custom #'custom-id)
	(create-column-name fb COL-NAME "Filename" :custom #'custom-name :expand t)
	(create-column fb COL-SIZE "Size" :align 1.0 :custom #'custom-size)
	(create-column fb COL-DATE "Mod" :custom #'custom-date)
	(create-column fb COL-Q    "Q" )
	(create-column fb COL-DIR "DIR" :custom #'custom-id)
	(create-column-icon fb COL-ICON "Icon")))

(defun create-filebox-widget (fb)
  "create gtk widget"
  (let ((widget (make-instance 'gtk-tree-view  :model (filebox-store fb))))
    (loop for column in (create-columns fb) do
	 (gtk-tree-view-append-column widget column))
    
    (gtk-tree-view-set-rules-hint widget 1) ;display stripes
      					; (fb-signal-connect selection "changed" on-selection-changed (selection))
      ;;invisible columns
    (gtk-tree-view-column-set-visible (gtk-tree-view-get-column widget COL-ID) nil)
    (gtk-tree-view-column-set-visible (gtk-tree-view-get-column widget COL-DIR) nil)
    (gtk-tree-view-column-set-visible (gtk-tree-view-get-column widget COL-Q) nil)
    (gtk-tree-view-column-set-visible (gtk-tree-view-get-column widget COL-ICON) nil)
      ;;
    (gtk-tree-view-enable-grid-lines widget )
    (gtk-tree-view-set-reorderable widget nil)
    (setf (gtk-widget-can-focus widget) t)
    (setf (gtk-tree-view-enable-search widget) nil); prevent key eating search box


      
    (gdk-threads-add-idle #'(lambda ()
					;   (format t "IDLE..." )
					;   (sleep 10)
					;   (format t "IDLE...>~%" )
			      nil  ))
    widget))
