(in-package :cl-fm)
;;; Keyboard
;;;
;;; keysym is the hardware keycode from GTK
;;;
#|(:shift-mask   #.(ash 1 0))
  (:lock-mask    #.(ash 1 1))
  (:control-mask #.(ash 1 2))
  (:mod1-mask    #.(ash 1 3))
  (:mod2-mask    #.(ash 1 4))
  (:mod3-mask    #.(ash 1 5))
  (:mod4-mask    #.(ash 1 6))
  (:mod5-mask    #.(ash 1 7))
  (:button1-mask #.(ash 1 8))
  (:button2-mask #.(ash 1 9))
  (:button3-mask #.(ash 1 10))
  (:button4-mask #.(ash 1 11))
  (:button5-mask #.(ash 1 12))
  (:super-mask   #.(ash 1 26))
  (:hyper-mask   #.(ash 1 27))
  (:meta-mask    #.(ash 1 28))
  (:release-mask #.(ash 1 30))
  (:modifier-mask #x5c001fff))
|#
(defstruct key
  keysym shift control meta alt hyper super)

(defparameter *modifier-keys* nil
  "List of modifier keysyms"
 )

(defun keysym-char (keysym)
  (and (< keysym char-code-limit)
       (code-char keysym)))

(defun print-key (key)
  "Convert a key into a string representation"
  (concatenate 'string
               (when (key-control key) "C-")
               (when (key-meta key) "M-")
               (when (key-alt key) "A-")
               (when (key-shift key) "S-")
               (when (key-super key) "s-")
               (when (key-hyper key) "H-")
	       (keysym->keysym-name (key-keysym key))))

(defun on-key-press (widget event)
  "Process a key from GTK; return key structure or nil for special keys"
  (declare (ignore widget))
  (let* ((keysym (gdk-event-key-keyval event)))
    ;;(format t "~A~%" (keysym->keysym-name keysym))
    (unless (member keysym *modifier-keys*)
      (let ((state (gdk-event-key-state event))
	    (key (make-key :keysym keysym)))
	(loop for modifier in state do
	     (case modifier 
	       (:shift-mask   (setf (key-shift key) t))
	       (:control-mask (setf (key-control key) t))
	       (:mod1-mask    (setf (key-meta key) t))))
	(let ((char (keysym-char keysym))) ;may be nil!
	  (and char
	       (key-shift key)
	       (upper-case-p char)
	       (setf (key-shift key) nil) 
	       ))
	(format t "~A~%" (print-key key)))
      ))
  
  t)


(defun keystroke-setup (widget)
  (setf *modifier-keys*
	(mapcar #'keysym-name->keysym
		(list "Shift_L"
		      "Shift_R"
		      "Control_L"
		      "Control_R"
		      "Meta_L"
		      "Meta_R"
		      "Super_L"
		      "Super_R"
		      "Alt_L"
		      "Alt_R"
		      "Caps_Lock"
		      "Shift_Lock")))
     ;; wiring
    (g-signal-connect widget "key-press-event" 'on-key-press)
   ; (g-signal-connect widget "key-release-event" 'on-key-release)
)






