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


(defun uris->pathstrings (uris)
  "convert a uri list from dnd to pathstrs. TODO: more portable, please!"
  (loop for f in uris
     collect (subseq f 7)) ) 

(defun sel->pathstrings (selected fb)
  "convert a dnd selection to a list of pathstrs of files in prefix dir."
  ;;; Currently, uris are in the form of "file:///...
  (loop for tpath in selected ;selected is a list of tree paths..
     collect (fb-full-namestring fb (model-path->name (filebox-store fb) tpath))))

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
	 (path (gtv-get-path-at-pos widget x y))
	 (sel (gtv-get-selection widget))
	 ;;(model (gtv-get-model widget))
	 ;;(iter (gtk-tree-model-get-iter model path))
	 )
    (enable-selection sel nil);disallow selection!
    ;; dragging is enabled if clicked on a selection
    (setf *drag-allowed* (gtk-tree-selection-path-is-selected sel path))
    )
  nil)

(defun on-drag-begin (widget context)
  (declare (ignore context))
  (setf *dragged-p*  (gtv-get-selection widget))
  (format t "DRAG BEGIN. DRAG ALLOWED: ~A~%" *drag-allowed*)
  (setf *dragged-onto* nil)
  (gtk-drag-source-set-icon-pixbuf widget (if *drag-allowed*
					      *pix-docs*
					      *pix-bad*)))
(defun on-button-release (widget event)
   (format t "BUTTON-RELEASE ~A ~%" (gdk-event-button-state event) )
  (let* ((x (round (gdk-event-button-x event)))
	 (y (round  (gdk-event-button-y event)))
	 (path (gtv-get-path-at-pos widget x y))
	 (sel (gtv-get-selection widget))
	 ;;(model (gtv-get-model widget))
	 ;;(iter (gtk-tree-model-get-iter model path))
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

;;(defun on-drag-data-get (widget context data info time))
  ;;; setting data does not work - cl-cffi-gtk bug.
 ;; (format t "DRAG-DATA-GET ~A~%" info)
 ;; (format t "Drag1: ~A~%" (gtk-drag-dest-find-target widget context))
 ;; (gtk-selection-data-set-uris data (list "file:///media/stacksmith/INTERNAL/downloads/GTK_dragndrop-1.pdf"))
  
;; cl-cffi-gtk gets suggested action as a list of strings (wtf)?
;; in reality it is a bitmask, although it seems not likely that
;; a combination of actions is possible...


(defun on-drag-data-received (widget context x y data info time )
  "retreive data from a successful (external) drop"
  (declare (ignore widget x y info))
  ;(format t "DRAG-DATA-RECEIVED~%")
  (let ((action (car (gdk-drag-context-get-suggested-action context)))
	(file-list (uris->pathstrings (gtk-selection-data-get-uris data))))
    (file-action action file-list)
    (gtk-drag-finish context t nil time )
    t))

(defun on-drag-failed (widget context result)
  (declare (ignore widget context result))
  (setf *dragged-onto* nil)
  (format t "DRAG FAILED~%"))



(defun on-drag-motion (widget context x y time)
  "return T if status set, nil if drop not permitted"
  ;(format t "DRAG-MOTION allowed:~A dragged-p:~A~%" *drag-allowed* *dragged-p*)
  (let ((path (gtv-get-dest-row-at-pos widget x y)))
    (if path
	(let((model (gtv-get-model widget)))
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
		    (gtv-set-drag-dest-row widget path :into-or-after)
		    (gdk-drag-status context (gdk-drag-context-get-suggested-action context) time)			 ;; for workaround, track destination id
		    (setf *dragged-onto* (gtk-tree-model-get-value model iter COL-ID))
		    ;;(format t "ONTO ~A~%" *draged-onto*)
		    t)
		  nil))))
	;otherwise, dragging over something other than the treeview
	nil)))

(defun on-drag-drop (fb widget context x y time)
  (declare (ignore x y))
					;  (format t "DRAG-DROP ~A ~%" *dragged-onto*)
  (let ((action (car (gdk-drag-context-get-suggested-action context))))
    (if (string= "NONE" action)
	(progn ;no common data target format, abort drop
	  (gtk-drag-finish context nil nil time ) nil)
	(if *dragged-p* ;we dragged it...
	    (if *drag-allowed*
		(progn ;dragged, allowed
		  (let ((selected (gtk-tree-selection-get-selected-rows *dragged-p* ))
			(model (filebox-store fb)))
		    (file-action action (sel->pathstrings selected fb)))
		  (gtk-drag-finish context t nil time ) t)
		(progn ; dragged, not allowed
		  (gtk-drag-finish context nil nil time ) nil))
	    ;; we didn't drag so someone else did...
	    (let ((target (gtk-drag-dest-find-target widget context)))
	      (if (string= "NONE" target)
		  (progn (gtk-drag-finish context nil nil time ) nil)
		  ;;(format t "TARGET ~A TYPE ~A~%" target (type-of target))
		  (progn
		    ;;this will issue a drag-data-received and it will finish
		    (gtk-drag-get-data widget context target time)
		    t)))))))
 


(defun on-drag-end (widget context )
  (declare (ignore widget))
  (format t "DRAG-END ~A~%" context)
  (setf *dragged-p* nil))
 

(defun drag-and-drop-setup (fb )
  (with-slots ((view widget)) fb
    (gtk-drag-dest-set view 0 *dnd-target-dest*
		       '(:copy :move :link :private :ask))		 
    (gtk-drag-source-set view :button1-mask *dnd-target-src*
			 '(:copy :move :link :private :ask)) 
    (g-signal-connect view "button-press-event" #'on-button-press)
    (g-signal-connect view "button-release-event" #'on-button-release)
    ;;DRAG
					;(g-signal-connect view "drag-data-get" #'on-drag-data-get)
    (g-signal-connect view "drag-begin" #'on-drag-begin)
    (g-signal-connect view "drag-data-received" #'on-drag-data-received)
    ;; (g-signal-connect view "drag-data-get" #'on-drag-data-get)      
    (fb-signal-connect view "drag-drop" on-drag-drop (widget context x y time))
    (g-signal-connect view "drag-end"    #'on-drag-end)
    (g-signal-connect view "drag-failed" #'on-drag-failed)
    (g-signal-connect view "drag-motion" #'on-drag-motion)
    ;;DROP
    
    (g-signal-connect view "changed" (lambda (sel) (format t "SEL CHANGED ~A~%" sel)) )))



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
