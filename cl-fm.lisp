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
(use-package 'eli)
(defparameter *fb* nil)


(defun app-set-path (eli)
  (declare (ignore eli))
  (filebox-set-path *fb* "/home/stacksmith/Downloads/")
  t)

(defun  app-up (eli)
  (declare (ignore eli))
  (filebox-up *fb*))

(defun app-q (fb num)
  (foreach-selected-row
   fb (lambda (model path iter)
	(model-set-q model path iter (filebox-path fb) num))))

(defun app-q-1 (eli) (app-q (eli:eli-payload eli) 1))
(defun app-q-2 (eli) (app-q (eli:eli-payload eli) 2))
(defun app-q-3 (eli) (app-q (eli:eli-payload eli) 3))
(defun app-q-4 (eli) (app-q (eli:eli-payload eli) 4))
(defun app-q-5 (eli) (app-q (eli:eli-payload eli) 5))

(defun tester (eli) (declare (ignore eli))
   (format t "xxx:~A~%" (gdk-screen-get-root-window (gdk-screen-get-default))  )
  )

(defun bind-keys (eli)
  (with-accessors ((keymap-top eli-keymap-top) (keymap-instant eli-keymap-instant)) eli
    (bind keymap-top "<C-x><C-f>" #'app-set-path)
    (bind keymap-top  "^" #'app-up)
    (bind keymap-top "<LEFT>" #'app-up)
    (bind keymap-top "<PAGE-DOWN>" #'tester)

    (bind keymap-top "<C-1>" #'app-q-1)
    (bind keymap-top "<C-2>" #'app-q-2)
    (bind keymap-top "<C-3>" #'app-q-3)
    (bind keymap-top "<C-4>" #'app-q-4)
    (bind keymap-top "<C-5>" #'app-q-5)
    
    )
  )

(defun  test (&key (dir "/media/stacksmith/DiskA/Trash/") (stdout *standard-output*))
  
  (within-main-loop
    (setf *standard-output* stdout) ;enable output in this thread
    (setf *window* (make-instance 'gtk-window
				  :title "cl-fm"
				  :type :toplevel
				  :border-width 0
				  :default-width 640
				  :default-height 480))
    (let* ((contents (make-instance 'gtk-box :orientation :vertical ))
	   (scrolled (make-instance 'gtk-scrolled-window
				    :border-width 3
				    :hscrollbar-policy :automatic
				    :vscrollbar-policy :automatic))
	   (fb (create-filebox dir *window*))
	   (eli (make-eli *window* fb)))
      
      (gtk-container-add scrolled (filebox-widget fb ))
      (gtk-box-pack-start contents scrolled)
      (gtk-box-pack-end contents (eli-bar eli) :expand nil)
      (setf *fb* fb)
      (gtk-container-add *window* contents)
      
      (g-signal-connect *window* "destroy"
			(lambda (widget)
			  (declare (ignore widget))
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
