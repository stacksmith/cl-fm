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



(defstruct filebox widget store path)
  
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
  (declare (ignore column renderer model iterator))

)

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

;; drag and drop support
;; drag-data-get (GtkWidget signal) for sourcing data during dnd
;;----------------
#|
static void multidrag_make_row_pixmaps(GtkTreeModel attribute((unused)) *model,
				       GtkTreePath *path,
				       GtkTreeIter *iter,
				       gpointer data) {
  struct multidrag_begin_state *qdbs = data;

  if(qdbs->predicate(path, iter)) {
    qdbs->pixmaps[qdbs->index++]
      = gtk_tree_view_create_row_drag_icon(qdbs->view, path);
  }
}
|#

(defparameter *click-x* 0)
(defparameter *click-y* 0)

(defparameter *drag-allowed* nil)
(defparameter *dragged-p* nil)
(defparameter *clicked-on* nil)


(defun on-button-press (widget event)
  (setf *dragged-p* nil)
  (format t "BUTTON-PRESS ~A ~%" (gdk-event-button-state event) )
  (let* ((x (round (gdk-event-button-x event)))
	 (y (round  (gdk-event-button-y event)))
	 (path (gtk-tree-view-get-path-at-pos widget x y))
	 (sel (gtk-tree-view-get-selection widget))
	 (model (gtk-tree-view-get-model widget))
	 (iter (gtk-tree-model-get-iter model path)))
    (setf *clicked-on* (gtk-tree-model-get-value model iter COL-ID))
					;(gtk-tree-view-set-cursor widget path)
    (gtk-tree-selection-set-select-function sel (lambda (sel model path selp)
						  (declare (ignore sel model path selp))
						  nil)) ;disallow selection!
    ;; dragging is enabled if clicked on a selection
    (setf *drag-allowed* (gtk-tree-selection-path-is-selected sel path))
    )
  t)

(defun on-button-release (widget event)
   (format t "BUTTON-RELEASE ~A ~%" (gdk-event-button-state event) )
  (let* ((x (round (gdk-event-button-x event)))
	 (y (round  (gdk-event-button-y event)))
	 (path (gtk-tree-view-get-path-at-pos widget x y))
	 (sel (gtk-tree-view-get-selection widget))
	 (model (gtk-tree-view-get-model widget))
	 (iter (gtk-tree-model-get-iter model path))
	 (released-on (gtk-tree-model-get-value model iter COL-ID)))
     (format t "RELEASED-ON ~A~%" released-on)  
     (gtk-tree-selection-set-select-function sel (lambda (sel model path selp) t)) ;unblock selection
     (let ((state (logand 15 (gdk-event-button-state event)))) ;bit 8 is 1 for release
       (if (not *dragged-p*)
	   (case state
	     (0 ; no modifiers
	      (gtk-tree-selection-unselect-all sel)
	      (gtk-tree-selection-select-path sel path))
	     (1 ; shift
	      (format t "AAA~%")
	      (gtk-tree-selection-select-range sel path (first (gtk-tree-selection-get-selected-rows sel))))
	     (4 ; control
	      (if (gtk-tree-selection-path-is-selected sel path) 
		  (gtk-tree-selection-unselect-path sel path) ;if selected, unselect
		  (gtk-tree-selection-select-path sel path)) ;if unselected, select
	      )))))
  (setf *dragged-p* nil)
  t)

(defun on-drag-data-get (widget context data info time)
  ;;; NEVER HAPPENS HERE
  (format t "DRAG-DATA-GET ~A~%" info)
  (format t "Drag1: ~A~%" (gtk-drag-dest-find-target widget context))
 ; (gtk-selection-data-set-uris data (list "file:///media/stacksmith/INTERNAL/downloads/GTK_dragndrop-1.pdf")) 
					;  (format t "~A~%" (type-of data))
					;(format t "ON-DRAG-DATA-GET~%~A~%~A from set~% " data)
					;  (format t "AAA ~%")
;					 (gtk-selection-data-set-text data "FUCK" )


 (let ((target (gtk-selection-data-target data))
	(uri "file:///media/stacksmith/INTERNAL/downloads/GTK_dragndrop-1.pdf"))
    (setf (gtk-selection-data-type data) target )
    (setf (gtk-selection-data-format data) 8)
    (setf (gtk-selection-data-data data) (cffi:foreign-string-alloc uri))
    (setf (gtk-selection-data-length data) (length uri)))
 ; (format t "TARGET ~A~%" data)

)  
					;
  
					;
  
  

  
  
					;  (format t "setting uris: ~A~%"	  (gtk-selection-data-set-uris data  (list "file:///media/stacksmith/INTERNAL/downloads/GTK_dragndrop-1.pdf")))
					;  (cffi:lisp-string-to-foreign "aaaabbbb                           " (gtk-selection-data-get-data data)  66)
					;(format t "~A~%" data)
					;(format t "~A~%" (gtk-selection-data-get-data data))
					;  (format t "~A~%" (cffi:foreign-string-to-lisp (gtk-selection-data-get-data data) :count 66))
  
					;(format t "----~A----~%~%" (gtk-selection-data-get-uris data))
					;  (format t "~A~%" data)
					;(format t "BBB~%")
  
					;  (format t "~A~%" data)
					;  (format t "~A~%~%"	    (gtk-selection-data-get-uris data ))
					;  (format t "~%~%~%" )
					;  data

  #|(define-g-flags "GdkDragAction" gdk-drag-action
  (:export t
  :type-initializer "gdk_drag_action_get_type")
  (:default 1)
  (:copy 2)
  (:move 4)
  (:link 8)
  (:private 16)
  (:ask 32))

  (define-g-flags "GtkTargetFlags" gtk-target-flags
  (:export t
  :type-initializer "gtk_target_flags_get_type")
  (:same-app 1)
  (:same-widget 2)
  (:other-app 4)
  (:other-widget 8))

  |#

(defun on-drag-data-received (widget context x y data info time )
  (format t "DRAG-DATA-RECEIVED~%")
					;  (format t " ~A ~A ~A ~A ~A ~A ~A~%" widget context x y data info etime)
					;  (format t "~A~%~%"  (cffi:convert-from-foreign (gtk-selection-data-get-data selection) :string ))
					;  (format t "~A~%" (cffi:foreign-string-to-lisp (gtk-selection-data-get-data data) :count 66))
  
  (format t "----~A----~%~%" data)
  (format t "----~A----~%~%" (gtk-selection-data-get-uris data))
;  (format t "----~A----~%~%" (gdk-atom-intern "text/uri-list" t))
 ; (gtk-drag-finish context t nil time)
					;  (format t "++++~A~%" (cffi:foreign-string-to-lisp (gtk-selection-data-get-data data) :offset 0 :count (gtk-selection-data-get-length data)))
;  (gtk-selection-convert widget data "text/uri-list" etime)
  )

 #|   
	  
      
		       0;(if isdir :COPY 0)
		       time)
      (format t "TPATH ~A POS ~A [~A]~%" path pos isdir)
      )
|#
(defun on-drag-failed (widget context result)
  (setf *dragged-onto* nil)
  (format t "DRAG FAILED~%"))

(defun on-drag-begin (widget context)
  (setf *dragged-p* t)
  (format t "DRAG BEGIN. DRAG ALLOWED: ~A~%" *drag-allowed*)
  (setf *dragged-onto* nil)
  
  (let*((model (gtk-tree-view-get-model widget))
	;;(selection (gtk-tree-view-get-selection widget))
	;;(selected (gtk-tree-selection-get-selected-rows selection ))
)
    (if *drag-allowed*
	(gtk-drag-source-set-icon-pixbuf widget *pix-docs*)
	(gtk-drag-source-set-icon-pixbuf widget *pix-bad*)
	;(gtk-drag-cancel )
	)
    
;    (format t "~A~%" selected)
    ;; Draw a cairo surface, then convert to pixbuf
#|    (let* ((surface (cairo-image-surface-create :argb32 160 160))
	   (cr (cairo-create surface))) 
      (cairo-move-to cr 0 0)
      (cairo-set-line-width cr 3.0)
      (cairo-set-source-rgba  cr 0.0 0.0 1.0 1.0)
      (cairo-line-to cr 150 150)
      (cairo-stroke cr)
      (cairo-destroy cr)
      (setf pp (gdk-pixbuf-get-from-surface surface 0 0 53 33) )
      (gtk-drag-source-set-icon-pixbuf widget pp)
      (cairo-surface-destroy surface))
  |#

;;    
    ;(format t "~A~%" icon)
    ))

					;	(format t "~A~%" (first selected))
;      (format t "(~A )~%" (cairo-xlib-surface-get-width icon))
   
      ;;allow drop into directries only

     ;nil means drop disallowed
     
;
 ;; (let (
					;(gtk_tree_view_create_row_drag_icon )
;    

  



(defun on-drag-motion (widget context x y time)
  "return T if status set, nil if drop not permitted"
  (format t "DRAG-MOTION ~A (~A,~A)~%" widget x y)
  (let*((path (gtk-tree-view-get-dest-row-at-pos widget x y)) ;not interested in pos
	(model (gtk-tree-view-get-model widget))
	(iter (gtk-tree-model-get-iter model path))
	(isdir (= 1 (gtk-tree-model-get-value model iter COL-DIR))))
    ;;allow drop into directries only, and then :into-or-after ignoring pos.
    (when isdir ;t means drop allowed, otherwise nil
      (format t "DIR: drop allowed~%")
      (gtk-tree-view-set-drag-dest-row widget path :into-or-after)
      ;; UNIMPLEMENTED in cl-cffi-gtk: dgk-drag-context-get-suggested-action
      (gdk-drag-status context (gdk-drag-context-get-suggested-action context) time)
          ;; for workaround, track destination id
      (setf *dragged-onto* (gtk-tree-model-get-value model iter COL-ID))
      t))) 


      

(defun on-drag-drop (widget context x y time)
  (format t "DRAG-DROP ~A (~A,~A)~%" widget x y)
  (gtk-drag-finish context t nil time )
  (multiple-value-bind (tpath pos) (gtk-tree-view-get-dest-row-at-pos widget x y)
    ()
    )
  
;  (format t "~A~%"  (gtk-drag-get-data widget context  (gtk-drag-dest-find-target widget context) time ))
;  (gtk-drag-finish context t nil time)
)

(defun on-drag-end (widget context )
  (format t "DRAG-END ~A~%" context))

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
					;    (gtk-tree-view-set-attributes )
					;    (gtk-drag-dest-add-uri-targets view)
    (let ((targets (vector
					; (gtk-target-entry-new "text/html" 0 112)
		    (gtk-target-entry-new "text/uri-list" 0 115)
		    ;(gtk-target-entry-new "GTK_TREE_MODEL_ROW" 0 110) ;putting this on top makes default work
		   
					; (gtk-target-entry-new "TEXT" 0 113)
					; (gtk-target-entry-new "STRING" 0 114)
		    )
	    ))
     ;(gtk-tree-view-enable-model-drag-dest view targets '(:copy :move :link :private :ask)) ;:private :ask
     ;(gtk-tree-view-enable-model-drag-source view :button1-mask targets '(:copy :move :link :private :ask ))
      (gtk-drag-dest-set view 0 targets '(:copy :move :link :private :ask))			 
      (gtk-drag-source-set view :button1-mask targets '(:copy :move :link :private :ask)) 


      (g-signal-connect view "button-press-event" #'on-button-press)
      (g-signal-connect view "button-release-event" #'on-button-release)
      ;;DRAG
					;(g-signal-connect view "drag-data-get" #'on-drag-data-get)
      (g-signal-connect view "drag-begin" #'on-drag-begin)
      (g-signal-connect view "drag-data-received" #'on-drag-data-received)
      (g-signal-connect view "drag-data-get" #'on-drag-data-get)      
      (g-signal-connect view "drag-drop" #'on-drag-drop)
      (g-signal-connect view "drag-end"    #'on-drag-end)
      (g-signal-connect view "drag-failed" #'on-drag-failed)
      (g-signal-connect view "drag-motion" #'on-drag-motion)
      ;;DROP

      (g-signal-connect view "changed" (lambda (sel) (format t "SEL CHANGED ~A~%" sel)) )
      
      )
    
    view))

(defun filebox-reload (fb)
  "reload all data"
  (model-refill (filebox-store fb) (filebox-path fb) :include-dirs t  ) 
  (model-postprocess (filebox-store fb) (filebox-path fb))
  )
(defun create-filebox (path)
  (let ((fb (make-filebox :path path
			  :store (create-model))))
    (setf (filebox-widget fb)
	  (create-filebox-widget (filebox-store fb))) 

;    (model-init (filebox-store fb))
    ;; wiring
    (g-signal-connect
     (filebox-widget fb) "key-press-event"
     (lambda (tv eventkey)
       (declare (ignore tv))
       (format t "KEYPRESS: [~X ~A]~%" (gdk-event-key-keyval eventkey) (gdk-event-key-keyval eventkey))
       (let ((keyval (gdk-event-key-keyval eventkey)))
	 (cond
	   ((eql keyval GTK-KEY-F3)
	    (foreach-selected-file fb (lambda (filename) (format t "~A~%" filename))) )
	   ((eql keyval GTK-KEY-F5) (format t "OK~%")  (filebox-reload fb) )
	   ;; range between 0 and 9
	   ((and (>= keyval #x30)
		 (<= keyval #x39))
	    (format t "0~%")
	    (foreach-selected-file
	     fb
	     (lambda (model path iterator)
	       (model-set-q model path iterator (filebox-path fb) (- keyval #x30))))
					;(filebox-reload fb)
	    )
	   (t (format t "NOPE")))
	 )
       t ;do not propagate
       ))
    ;; Double-click
    (g-signal-connect
     (filebox-widget fb) "row-activated"
     (lambda (tv path column )
       (format t "~A ~A ~A  ~%" tv path column)
       (let* ((model (gtk-tree-view-get-model tv))
	      (iter (gtk-tree-model-get-iter model path))
	      ;(sel (gtk-tree-view-get-selection tv))
	      (path (merge-pathnames (filebox-path fb)
				     (gtk-tree-model-get-value model iter COL-NAME))))
					;	 (format t "ITER ~A ~%" path )
					;TODO: figure out application
	 (external-program:start "vlc" (list path))
	 ;; selected
;	 (gtk-tree-selection-selected-foreach sel (lambda (mod path iter) (format t "MULT SEL ~A~%" (gtk-tree-model-get-value mod iter COL-ID))))
	 )))
     (filebox-reload fb) ;initial load
    fb)
  )



;-----------------------
