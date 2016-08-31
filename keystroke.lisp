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
(defconstant MOD-CONTROL-BIT 0)
(defconstant MOD-META-BIT 1)
(defconstant MOD-ALT-BIT 2)
(defconstant MOD-SHIFT-BIT 3)
(defconstant MOD-SUPER-BIT 4)
(defconstant MOD-HYPER-BIT 5)

(defconstant MOD-CONTROL-MASK (ash 1 MOD-CONTROL-BIT) )
(defconstant MOD-META-MASK    (ash 1 MOD-META-BIT) ) 
(defconstant MOD-ALT-MASK     (ash 1 MOD-ALT-BIT) ) 
(defconstant MOD-SHIFT-MASK   (ash 1 MOD-SHIFT-BIT) ) 
(defconstant MOD-SUPER-MASK   (ash 1 MOD-SUPER-BIT) ) 
(defconstant MOD-HYPER-MASK   (ash 1 MOD-HYPER-BIT) )


(defstruct key keysym mod)

(defparameter *modifier-keys* nil
  "List of modifier keysyms" ;initialized in keystroke setup..
 )

(defun keysym-char (keysym)
  (and (< keysym char-code-limit)
       (code-char keysym)))

(defun print-key (key)
  "Convert a key into a string representation"
  (let ((mod (key-mod key)))
    (concatenate 'string
		 (when (logbitp mod-control-bit mod) "C-")
		 (when (logbitp mod-meta-bit    mod) "M-")
		 (when (logbitp mod-alt-bit     mod) "A-")
		 (when (logbitp mod-shift-bit   mod) "S-")
		 (when (logbitp mod-super-bit   mod) "s-")
		 (when (logbitp mod-hyper-bit   mod) "H-")
		 (keysym->keysym-name (key-keysym key)))))
;;;
;;; We only care about control and meta (alt key).
;;; Shift is already processed for us.
(defun gtk->modmap (gtk-modifiers)
  "scan the gtk-modifier list and create a mod bitmap"
  (let ((modmap 0))
    (dolist (modifier gtk-modifiers)
	(case modifier 
	  (:control-mask (incf modmap MOD-CONTROL-MASK))
	  (:mod1-mask    (incf modmap MOD-META-MASK))))
    modmap))

(defun on-key-press (widget event)
  "Process a key from GTK; return key structure or nil for special keys"
  (declare (ignore widget))
  (let ((keysym (gdk-event-key-keyval event)))
    (unless (member keysym *modifier-keys*) ;skip modifier keypresses
      (format t "~A~%" 
	      (print-key  (make-key :keysym keysym :mod (gtk->modmap (gdk-event-key-state event)))))))  
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






