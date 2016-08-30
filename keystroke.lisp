(in-package :cl-fm)
;;; Keyboard
;;;
;;; keysym is the hardware keycode from GTK
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

(defstruct key
  keysym shift control meta alt hyper super)

(defun modifier-mask (keysym)
  "given a keysym, return modifier mask if it is a modifier or nil otherwise"
  (cond
    ((eql keysym GTK-KEY-SHIFT)    MOD-SHIFT-MASK)
    ((eql keysym GTK-KEY-CONTROL)  MOD-CONTROL-MASK)
    ((eql keysym GTK-KEY-META)     MOD-META-MASK)
    (t nil)))
(let ((modifier 0)) 

  (defun add-modifiers (keysym)
    "if keysym is a modifier key, update modifier bitmap and return non-nil"
    (let ((xmod (modifier-mask keysym)))
      (when xmod (setf modifier (logior modifier xmod)))
      xmod))
  
  (defun remove-modifiers (keysym)
    "if keysym is a modifier key, update modifier bitmap and return non-nil"
    (let ((xmod (modifier-mask keysym)))
      (when xmod (setf modifier (logand modifier (lognot xmod))))      
      xmod))

  
  (defun on-key-release (widget event)
    (declare (ignore widget))
    (let ((keysym (gdk-event-key-keyval event)))
      (remove-modifiers keysym)
      ;; (format t "KEY-RELEASE MOD ~X~%" modifier)
      t))
  
  (defun on-key-press (widget event)
    (declare (ignore widget))

    (let ((keysym (gdk-event-key-keyval event)))
      (format t "KKKK: ~A~%" event)
      (unless (add-modifiers keysym)
	(format t "KEY-PRESS ~A mod: ~A~%" (keysym->keysym-name keysym) modifier)
	(cond
	  ((eql keysym GTK-KEY-F3)
	;;(foreach-selected-file fb (lambda (filename) (format t "~A~%" filename)))
	   )
	  ((eql keysym GTK-KEY-F5) (format t "OK~%")
	   ;;(filebox-reload fb)
	   )
	  ;; range between 0 and 9
	  ((and (>= keysym #x30)
		(<= keysym #x39))
	   (format t "0~%")
	;;       (foreach-selected-file fb (lambda (model path iterator) (model-set-q model path iterator (filebox-path fb) (- keysym #x30))))
	   ))))
    t))

(defun keystroke-setup (widget)
     ;; wiring
    (g-signal-connect widget "key-press-event" 'on-key-press)
    (g-signal-connect widget "key-release-event" 'on-key-release)
)



