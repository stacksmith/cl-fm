
(in-package :cl-fm)
(defconstant COL-ID 0)
(defconstant COL-NAME 1)
(defconstant COL-SIZE 2)
(defconstant COL-DATE 3)
(defconstant COL-Q 4)
(defconstant COL-DIR 5)
(defconstant COL-ICON 6)


(defparameter *icon-folder* (gdk-pixbuf-new-from-file (namestring (asdf:system-relative-pathname
								'cl-fm "resources/icon-folder.png"))))

(defun create-model ()
  (let ((model
	 (make-instance 'gtk-tree-store
			              ;;; ID        NAME       SIZE    DATE    Q      DIR     ICON
			:column-types '("guint" "gchararray" "gint64" "guint" "guint" "guint" "GdkPixbuf"))))
    (g-signal-connect model "row-deleted" #'on-row-deleted)
    (g-signal-connect model "row-inserted" #'on-row-inserted)
    (g-signal-connect model "row-changed" #'on-row-changed)
    model))

(defun model-append-initial-dir (store i path-base path-name)
  "append an initial directory entry"
  (gtk-tree-store-set
   store
   (gtk-tree-store-append store nil) ;iter
   i          ;ID
   (enough-namestring path-name path-base)
;   (concatenate 'string (car (last (pathname-directory file-name))) "/");;TODO: portability
   -1         ;SIZE
   0          ;DATE
   #xf        ;Q
   1
   *icon-folder*))

(defun model-append-initial-file (store i path-name)
  (gtk-tree-store-set
   store (gtk-tree-store-append store nil)
   i          ;ID
   (file-namestring path-name ) ;NAME
   -1         ;SIZE
   0          ;DATE
   #xf        ;Q
   0
   ))

(defun model-refill (store path &key (include-dirs t))
  "clear gtk store and reload store with data from filesystem"
  (gtk-tree-store-clear store)
  ;; First load directories, then files...
;  (format t "I: ~A~%" (uiop:subdirectories path))
  (let ((i 1))
    (and include-dirs
	 (loop for subdir-path in (uiop:subdirectories path); (cl-fad:list-directory path)
	    do
	      (model-append-initial-dir store i path subdir-path)
	      (incf i)))
    
    (loop for file-path in (uiop:directory-files path); (cl-fad:list-directory path)
       do
	 (model-append-initial-file store i file-path)
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
       (gtk-tree-store-set-value model iter COL-Q q)
      )
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
(defun model-set-name (model treepath value)
  (gtk-tree-store-set-value
   model
   (gtk-tree-model-get-iter model treepath)
   COL-NAME
   value))

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


