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


(defun app-quit (eli)
  (declare (ignore eli))
  (gtk-widget-destroy *window*)
  (format t "quit done~%")
;  (gtk-main-quit)
;  (g-signal-emit *window* "delete-event")
  )
(defun app-set-path (eli)
  (filebox-set-path *fb* "/home/stacksmith/Downloads/")
  t)

(defun  app-up (eli)
  (filebox-up *fb*)
  )



(defun bind-keys (eli)
  (with-slots (eli:keymap-top eli:keymap-instant) eli
    (setf keymap-top   (new-keymap))
    (bind keymap-top  "C-xC-c" #'app-quit) 
    (bind keymap-top  "C-xC-f" #'app-set-path)
    (bind keymap-top  "^" #'app-up)

    (setf keymap-instant  (new-keymap))
    (bind keymap-instant  "C-g" #'inst-cancel)
    (bind keymap-instant "BS" #'inst-back-up)
    )
  )

(defun  test (&key (stdout *standard-output*))
  
  (within-main-loop
    (setf *standard-output* stdout) ;enable output in this thread
    (setf *window* (make-instance 'gtk-window
				  :title "cl-fm"
				  :type :toplevel
				  :border-width 0
				  :default-width 640
				  :default-height 480))
    (let ((contents (make-instance 'gtk-box :orientation :vertical ))
	  (scrolled (make-instance 'gtk-scrolled-window
				   :border-width 3
				   :hscrollbar-policy :automatic
				   :vscrollbar-policy :automatic))
	  (eli (make-bar *window*))
	  (fb (create-filebox "/media/stacksmith/DiskA/Trash/" *window*)))
      (gtk-container-add scrolled (filebox-widget fb ))
      (gtk-box-pack-start contents scrolled)
      (gtk-box-pack-end contents (eli::eli-bar eli) :expand nil)
      (setf *fb* fb)
      (gtk-container-add *window* contents)
      
      (g-signal-connect *window* "key-press-event" 'eli::on-key-press)

      (g-signal-connect *window* "destroy"
			(lambda (widget)
			  (declare (ignore widget))
			  (format t "done")
			  (leave-gtk-main)))
      (bind-keys eli)
      (gtk-widget-show-all *window*)
      (reset eli :full t))
    
    
    ))

#|
(g-signal-connect
 *window* "key-press-event"
 (lambda (widget ek) (declare (ignore widget))
	 (case (gdk-event-key-hardware-keycode ek)
	   (69 (inner-f3 fb)) ;F3
	   (t  (format t "KEY ~A~%" (gdk-event-key-hardware-keycode ek)))
	   )))
|#
