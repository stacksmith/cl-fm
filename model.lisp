
(in-package :cl-fm)
(defconstant COL-ID 0)
(defconstant COL-NAME 1)
(defconstant COL-SIZE 2)
(defconstant COL-DATE 3)
(defconstant COL-Q 4)


(defun create-column (number title &key (custom nil) (align nil) (scale 0.75))
  "helper - create a single column with a text renderer"
  (let* ((renderer (gtk-cell-renderer-text-new))
	 (column (gtk-tree-view-column-new-with-attributes title renderer "text" number)))
    (setf (gtk-cell-renderer-text-scale-set renderer) t) ;allow text to scale
    (setf (gtk-cell-renderer-text-scale renderer) scale)   ;scale a little smaller
    (when align (setf (gtk-cell-renderer-xalign renderer) align)) ;align data within cell
    (when custom (gtk-tree-view-column-set-cell-data-func ;custom renderer data
		  column renderer custom))
    (gtk-tree-view-column-set-sort-column-id column number)
    (gtk-tree-view-column-set-reorderable column t)
    column))

(defun create-columns ()
  ;; Create columns
  (list (create-column COL-ID "#" :align 1.0 :custom #'custom-id)
	(create-column COL-NAME "Filename" :custom #'custom-name)
	(create-column COL-SIZE "Size" :align 1.0 :custom #'custom-size)
	(create-column COL-DATE "Mod" :custom #'custom-date)
	(create-column COL-Q    "Q" )))

(defun create-model ()
  (make-instance 'gtk-list-store
		                 ;; ID        NAME       SIZE    DATE    Q
		 :column-types '("guint" "gchararray" "gint64" "guint" "guint")))

(defun model-refill (store path)
  "clear gtk store and reload store with data from filesystem"
  (gtk-list-store-clear store)
  (loop for file-name in (cl-fad:list-directory path)
     for i from 1 to 10000
     do (gtk-list-store-set store (gtk-list-store-append store)
			    i          ;ID
			    (file-namestring (string-right-trim "/" (namestring file-name) )) ;NAME
			    -1         ;SIZE
			    0          ;DATE
			    #xf        ;Q
			    )))

(defun model-postprocess (store directory)
  "across all files, update size, date and q"
  (format t "XXXXXXXXXXXXXXXXXXXXXX~%")
  (gtk-tree-model-foreach
   store
   (lambda (model path iter)
     (declare (ignore path))
     (let ((fname (merge-pathnames directory (gtk-tree-model-get-value model iter COL-NAME)))
	   
	   ) ;build full filepath
       ;(unless (cl-fad:directory-exists-p fname))
       (let ((size (with-open-file (in fname) (file-length in)))
	     (date (file-write-date fname))
	     (q (q-get fname)))
					;    (format t "~A \"~A\" ~A ~A ~A ~%" id name size date q)
	 (unless q (setf q #XF))
	 (if (or (< q 0) (> q 15)) (setf q #XF)) ;TODO: handle range check better !!!
	 (gtk-list-store-set-value model iter COL-SIZE size)
	 (gtk-list-store-set-value model iter COL-DATE date)
	 (gtk-list-store-set-value model iter COL-Q q))
;	 (gtk-list-store-set model iter  id name size date q )
	)
     nil)))


(defun model-set-q (model path iterator directory value )
  "called to modify q in file and model"
  (let ((pathname (merge-pathnames directory
				   (gtk-tree-model-get-value model iterator COL-NAME))))
    (q-set value pathname))
  (gtk-list-store-set-value model iterator COL-Q value)
  (gtk-tree-model-row-changed model path iterator ))


(defun model-row-changed (model tp iter)
  (format t "~A ROW-CHANGED ~A~%" (get-universal-time) (first (gtk-tree-model-get model iter COL-ID)
							      )))

(defun model-init (model)
  (g-signal-connect model "row-changed" #'model-row-changed))
