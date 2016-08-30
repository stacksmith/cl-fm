(in-package :cl-fm)
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



(defun modifier-mask (keyval)
  "given a keyval, return modifier mask if it is a modifier or nil otherwise"
  (cond
    ((eql keyval GTK-KEY-SHIFT)    MOD-SHIFT-MASK)
    ((eql keyval GTK-KEY-CONTROL)  MOD-CONTROL-MASK)
    ((eql keyval GTK-KEY-META)     MOD-META-MASK)
    (t nil)))
(let ((modifier 0)
  ) 

  (defun add-modifiers (keyval)
    "if keyval is a modifier key, update modifier bitmap and return non-nil"
    (let ((xmod (modifier-mask keyval)))
      (when xmod (setf modifier (logior modifier xmod)))
      xmod))
  
  (defun remove-modifiers (keyval)
    "if keyval is a modifier key, update modifier bitmap and return non-nil"
    (let ((xmod (modifier-mask keyval)))
      (when xmod (setf modifier (logand modifier (lognot xmod))))      
      xmod))

  
  (defun on-key-release (widget event)
    (let ((keyval (gdk-event-key-keyval event)))
      (remove-modifiers keyval)
      (format t "KEY-RELEASE MOD ~X~%" modifier)
      t))
  
  (defun on-key-press (widget event)
    (let ((keyval (gdk-event-key-keyval event)))
      (format t "KEY-PRESS ~X~%" keyval)
      (unless (add-modifiers keyval)
        (cond
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
	   ))
    
	(format t "KEY-PRESS MOD ~X~%" modifier)))
    t))

(defun keystroke-setup (widget)
     ;; wiring
    (g-signal-connect widget "key-press-event" 'on-key-press)
    (g-signal-connect widget "key-release-event" 'on-key-release)
)



