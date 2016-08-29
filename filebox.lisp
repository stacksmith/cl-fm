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
    (drag-and-drop-setup view) ;see "drag-and-drop.lisp"
    
    view))

(defun filebox-reload (fb)
  "reload all data"
  (model-refill (filebox-store fb) (filebox-path fb) :include-dirs t  ) 
  (model-postprocess (filebox-store fb) (filebox-path fb))
  (format t "KEYPRESS: [~X ~A]~%" (gdk-event-key-keyval eventkey) (gdk-event-key-keyval eventkey))
  
  )

;;; Keyboard
;;;
;;; Due to a bug(?) https://github.com/crategus/cl-cffi-gtk/issues/46, modifier keys
;;; are turned into a list of keywords indicating modifiers, instead of a bitmask.
;;; So I am turning them back into a bitmask, pending a fix.

(defconstant GTK-KEY-F3 #XFFC0)
(defconstant GTK-KEY-F5 #XFFC2)
(defconstant GTK-KEY-SHIFT #XFFE1)
(defconstant GTK-KEY-CONTROL #XFFE3)
(defconstant GTK-KEY-META #XFFE9)

(defconstant MOD-SHIFT-MASK   (ash 1 0))
(defconstant MOD-CONTROL-MASK (ash 1 2))
(defconstant MOD-META-MASK    (ash 1 28))

(let ((modifier 0)) 

  (defun on-key-release (widget event)
    (let ((keyval (gdk-event-key-keyval event)))
      (cond
	((eql keyval GTK-KEY-SHIFT)
	 (setf modifier (logand modifier (lognot MOD-SHIFT-MASK))))
	((eql keyval GTK-KEY-CONTROL)
	 (setf modifier (logand modifier (lognot MOD-CONTROL-MASK))))
	((eql keyval GTK-KEY-META)
	 (setf modifier (logand modifier (lognot MOD-META-MASK))))
	)
      (format t "KEY-RELEASE MOD ~X~%" modifier))  
    )
  (defun on-key-press (widget event)
    (let ((keyval (gdk-event-key-keyval event)))
      (format t "KEY-PRESS ~X~%" keyval)
      (cond
	((eql keyval GTK-KEY-SHIFT)   (setf modifier (logior modifier MOD-SHIFT-MASK)))
	((eql keyval GTK-KEY-CONTROL) (setf modifier (logior modifier MOD-CONTROL-MASK)))
	((eql keyval GTK-KEY-META)    (setf modifier (logior modifier MOD-META-MASK)))
	 
	((eql keyval GTK-KEY-F3)
					;(foreach-selected-file fb (lambda (filename) (format t "~A~%" filename)))
	 )
	((eql keyval GTK-KEY-F5) (format t "OK~%")
					;(filebox-reload fb)
	 )
	;; range between 0 and 9
	((and (>= keyval #x30)
	      (<= keyval #x39))
	 (format t "0~%")
					;       (foreach-selected-file fb (lambda (model path iterator) (model-set-q model path iterator (filebox-path fb) (- keyval #x30))))
	 )))
    (format t "KEY-PRESS MOD ~X~%" modifier)
    nil
    ))
  

(defun create-filebox (path)
  (let ((fb (make-filebox :path path
			  :store (create-model))))
    (setf (filebox-widget fb)
	  (create-filebox-widget (filebox-store fb))) 

					;    (model-init (filebox-store fb))
    ;; wiring
    (g-signal-connect (filebox-widget fb) "key-press-event" 'on-key-press)
    (g-signal-connect (filebox-widget fb) "key-release-event" 'on-key-release)
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
  
