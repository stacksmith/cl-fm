;;;; cl-fm.lisp

(in-package #:cl-fm)
(defparameter *window* nil)
;; fb = filebox, the container displaying 1 or 2 file lists
(defparameter *x* nil)









#|x
(defun fb-create ()
  "inside the filebox"
  (let ((fb (make-fb)))
    (setf (fb-container fb) (make-instance 'gtk-box
					:orientation :horizontal
					:homogenous t
					:spacing 3)
#|	  (fb-box1 fb) (make-instance 'gtk-file-chooser-widget
				      :subtitle "okeydokey"
				      :search-mode nil
				      :preview-widget-active nil
				      ); action OPEN
 (gtk-file-chooser-set-current-folder (fb-box1 fb) "/media/stacksmith/TEMP/")
    (gtk-file-chooser-set-create-folders (fb-box1 fb) t)
|#
	  (fb-box1 fb) (make-instance 'gtk-label :label "b1")	 
	  )
    
   ; (fb-box2 fb) (make-instance 'gtk-label :label "b2")
 ;   (format t "URI ~A~%" (gtk-file-chooser-get-current-folder-uri (fb-box1 fb)))
    (gtk-box-pack-start (fb-container fb) (fb-box1 fb) :expand t :fill t)
    ;(gtk-box-pack-start (fb-container fb) (fb-box2 fb) :expand t :fill t)
    fb))

(defun fb-split (fb)
  "toggle visibilty of second pane"
  (if (gtk-widget-visible (fb-box2 fb))
      (gtk-widget-hide (fb-box2 fb))
      (gtk-widget-show (fb-box2 fb))))


|#

(defparameter *fb* nil)
(defun  test (&key (stdout *standard-output*))
  (let ((fb nil))
    (within-main-loop
      (setf *standard-output* stdout) ;enable output in this thread
      (setf *window* (make-instance 'gtk-window
				    :title "filebox"
				    :type :toplevel
				    :border-width 0
				    :default-width 640
				    :default-height 480))
      
      (let (;(w-select (new-window-select))
					;	  (split nil)
					;	  (notebook (make-instance 'gtk-notebook))
					;	  (tab-hbox1 (make-instance 'gtk-box   :orientation :horizontal))
					;	  (tab-label1 (make-instance 'gtk-label :label "Select"))
					;	  (tab-hbox2 (make-instance 'gtk-box   :orientation :horizontal))
					;	  (tab-label2 (make-instance 'gtk-label :label "Rename"))

					;
					;	  (page2 (make-instance 'gtk-label:label "crap2"))
	    )
					;     (gtk-box-pack-start tab-hbox1 tab-label1)
					;    (gtk-widget-show-all tab-hbox1)
					;   (gtk-box-pack-start tab-hbox2 tab-label2)
					;  (gtk-widget-show-all tab-hbox2)
	
					;      (gtk-notebook-add-page notebook (filebox-widget fb) tab-hbox1)
					;      (gtk-notebook-add-page notebook page2 tab-hbox2)
	
	(setf fb  (create-filebox "/media/stacksmith/DiskA/Trash/"))
	(setf *fb* fb)
	(gtk-container-add *window* (filebox-widget fb ))
	

	(g-signal-connect *window* "destroy"
			  (lambda (widget)
			    (declare (ignore widget))
			    (format t "done")
			    (leave-gtk-main)))
	
	(gtk-widget-show-all *window*)
	
	
	))))

#|
(g-signal-connect
 *window* "key-press-event"
 (lambda (widget ek) (declare (ignore widget))
	 (case (gdk-event-key-hardware-keycode ek)
	   (69 (inner-f3 fb)) ;F3
	   (t  (format t "KEY ~A~%" (gdk-event-key-hardware-keycode ek)))
	   )))
|#
