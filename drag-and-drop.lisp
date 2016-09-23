(in-package :cl-fm)
;;; TODO:
;;; - autoscrol on drag

;;; Icon resources.
(defparameter *pix-docs* (gdk-pixbuf-new-from-file (namestring (asdf:system-relative-pathname
								'cl-fm "resources/icon-docs.png"))))
(defparameter *pix-bad*  (gdk-pixbuf-new-from-file (namestring (asdf:system-relative-pathname
								'cl-fm "resources/icon-bad.png"))))
;;; Handling of selections (especially setting them) is currently broken in cl-cffi-gtk
;;; (as of Sept 2016).  We shall do the minimum here - use gtk-tree-view default handling,
;;; but intercept button clicks and do our own thing.
;;;
;;; This is likely to change as of next release...
;;;
(defparameter *dnd-target-src* (vector
				;;(gtk-target-entry-new "text/html" 0 112)
				(gtk-target-entry-new "text/uri-list" 0 115)
				;;(gtk-target-entry-new "GTK_TREE_MODEL_ROW" 0 110)
				;; (gtk-target-entry-new "TEXT" 0 113)
				;; (gtk-target-entry-new "STRING" 0 114)
				))
(defparameter *dnd-target-dest-int* (gtk-target-list-new ()))
(defparameter *dnd-target-dest* (vector
				;;(gtk-target-entry-new "text/html" 0 112)
				(gtk-target-entry-new "text/uri-list" 0 115)
				;;(gtk-target-entry-new "GTK_TREE_MODEL_ROW" 0 110)
				;; (gtk-target-entry-new "TEXT" 0 113)
				;; (gtk-target-entry-new "STRING" 0 114)
				))


(defparameter *drag-allowed* nil) ;disallow dragging unless something is selected
(defparameter *dragged-p* nil) ;set by drag-begin, reset by drag-end
(defparameter *dragged-onto* nil) ;drag target

;;; Treeview is bad with dragging multiple selections as clicking to drag will turn off
;;; the item clicked on prior to dragging.  We shall disallow the treeview from deselcting
;;; the selection for the drag...
;;;
(defun enable-selection (selection yesno)
  "set model callback to allow or disallow modification to selection"
  (gtk-tree-selection-set-select-function
   selection
   (lambda (sel model path selp) ;nil return means leave it unchanged...
     (declare (ignore sel model path selp))
     yesno)))
;;;
;;; We are changing the semantics of a press: only release selects/deselects...
;;;
(defun on-button-press (widget event)
  (setf *dragged-p* nil)
  (format t "BUTTON-PRESS ~A ~%" (gdk-event-button-state event) )
  (let* ((x (round (gdk-event-button-x event)))
	 (y (round  (gdk-event-button-y event)))
	 (path (gtk-tree-view-get-path-at-pos widget x y))
	 (sel (gtk-tree-view-get-selection widget))
	 (model (gtk-tree-view-get-model widget))
	 (iter (gtk-tree-model-get-iter model path)))
    (enable-selection sel nil);disallow selection!
    ;; dragging is enabled if clicked on a selection
    (setf *drag-allowed* (gtk-tree-selection-path-is-selected sel path))
    )
  nil)

(defun on-drag-begin (widget context)
  (setf *dragged-p* t)
  (format t "DRAG BEGIN. DRAG ALLOWED: ~A~%" *drag-allowed*)
  (setf *dragged-onto* nil)
  
  (let*((model (gtk-tree-view-get-model widget))
	;;(selection (gtk-tree-view-get-selection widget))
	;;(selected (gtk-tree-selection-get-selected-rows selection ))
	)
    ;; set appropriate icon here
    (gtk-drag-source-set-icon-pixbuf widget (if *drag-allowed*
						*pix-docs*
						*pix-bad*))))
(defun on-button-release (widget event)
   (format t "BUTTON-RELEASE ~A ~%" (gdk-event-button-state event) )
  (let* ((x (round (gdk-event-button-x event)))
	 (y (round  (gdk-event-button-y event)))
	 (path (gtk-tree-view-get-path-at-pos widget x y))
	 (sel (gtk-tree-view-get-selection widget))
	 (model (gtk-tree-view-get-model widget))
	 (iter (gtk-tree-model-get-iter model path))
	 ;; (released-on (gtk-tree-model-get-value model iter COL-ID))
	 )
    (enable-selection sel t)
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
  nil)

(defun on-drag-data-get (widget context data info time)
  ;;; setting data does not work - cl-cffi-gtk bug.
 ;; (format t "DRAG-DATA-GET ~A~%" info)
 ;; (format t "Drag1: ~A~%" (gtk-drag-dest-find-target widget context))
 ;; (gtk-selection-data-set-uris data (list "file:///media/stacksmith/INTERNAL/downloads/GTK_dragndrop-1.pdf"))
  )
   

(defun on-drag-data-received (widget context x y data info time )
  (declare (ignore widget context x y info time))
  (format t "DRAG-DATA-RECEIVED~%")
  (format t "----~A----~%~%" (gtk-selection-data-get-uris data)))

(defun on-drag-failed (widget context result)
  (declare (ignore widget context result))
  (setf *dragged-onto* nil)
  (format t "DRAG FAILED~%"))



(defun on-drag-motion (widget context x y time)
  "return T if status set, nil if drop not permitted"
  ;;(format t "DRAG-MOTION ~A (~A,~A)~%" widget x y)
  (let ((path (gtk-tree-view-get-dest-row-at-pos widget x y)))
    (if path
	(let((model (gtk-tree-view-get-model widget)))
	  ;;(gtk-drag-source-set-icon-pixbuf widget *pix-docs*)
	  ;;(format t "PATH: ~A~%" path)
	  (let ((iter (gtk-tree-model-get-iter model path) ))
	    ;; (format t "ITER: ~A~%" iter)
	    (let ((isdir (= 1 (gtk-tree-model-get-value model iter COL-DIR))))
	      ;;(format t "ISDIR: ~A~%" iter)
	      ;;allow drop into directries only, and then :into-or-after ignoring pos.
	      (if isdir	  ;t means drop allowed, otherwise nil
		  (progn
		    ;;(format t "DIR: drop allowed~%")
		    (gtk-tree-view-set-drag-dest-row widget path :into-or-after)
		    ;; UNIMPLEMENTED in cl-cffi-gtk: dgk-drag-context-get-suggested-action
		    (gdk-drag-status context (gdk-drag-context-get-suggested-action context) time)			 ;; for workaround, track destination id
		    (setf *dragged-onto* (gtk-tree-model-get-value model iter COL-ID))
		    (format t "ONTO ~A~%" *dragged-onto*)
		    t)
		  nil))))
	;otherwise, dragging over something other than the treeview
	nil)))

(defun on-drag-drop (widget context x y time)
  (format t "DRAG-DROP ~A (~A,~A)~%" widget x y)
 ; (multiple-value-bind (tpath pos) (gtk-tree-view-get-dest-row-at-pos widget x y)  )
  t  
  
  (format t "...~A~%"  (gtk-drag-get-data widget context  (gtk-drag-dest-find-target widget context) time ))
  (gtk-drag-finish context t nil time ))
 


(defun on-drag-end (widget context )
  (format t "DRAG-END ~A~%" context)
  (setf *dragged-p* nil))

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

(defun drag-and-drop-setup (view)
  (gtk-drag-dest-set view 0 *dnd-target-dest* '(:copy :move :link :private :ask))			 
  (gtk-drag-source-set view :button1-mask *dnd-target-src*  '(:copy :move :link :private :ask)) 
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
  
  (g-signal-connect view "changed" (lambda (sel) (format t "SEL CHANGED ~A~%" sel)) ))



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
