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
(defconstant MOD-CONTROL-BIT 24)
(defconstant MOD-META-BIT    25)
(defconstant MOD-ALT-BIT     26)
(defconstant MOD-SHIFT-BIT   27)
(defconstant MOD-SUPER-BIT   28)
(defconstant MOD-HYPER-BIT   29)

(defconstant MOD-CONTROL-MASK (ash 1 MOD-CONTROL-BIT) )
(defconstant MOD-META-MASK    (ash 1 MOD-META-BIT) ) 
(defconstant MOD-ALT-MASK     (ash 1 MOD-ALT-BIT) ) 
(defconstant MOD-SHIFT-MASK   (ash 1 MOD-SHIFT-BIT) ) 
(defconstant MOD-SUPER-MASK   (ash 1 MOD-SUPER-BIT) ) 
(defconstant MOD-HYPER-MASK   (ash 1 MOD-HYPER-BIT) )

(defparameter keyval-spec (byte 24 0))
(defparameter keymod-spec (byte 8 24))
(defmacro key-val (key)
  "return keyval of the key"
  `(ldb keyval-spec ,key))
(defmacro key-mod (key)
  "return mod flags of key, not settable"
  `(mask-field keymod-spec ,key))

(defparameter *modifier-keys* nil
  "List of modifier keysyms" ;initialized in keystroke setup..
 )

(defun key-char (key)
  (and (<  (key-val key) char-code-limit)
       (code-char (key-val key))))

(defun key-str (key)
  "Convert a key into a string representation"
    (concatenate 'string
	       (when (logbitp mod-control-bit key) "C-")
	       (when (logbitp mod-meta-bit    key) "M-")
	       (when (logbitp mod-alt-bit     key) "A-")
	       (when (logbitp mod-shift-bit   key) "S-")
	       (when (logbitp mod-super-bit   key) "s-")
	       (when (logbitp mod-hyper-bit   key) "H-")
	       (keysym->keysym-name (key-val key))))
;;;
;;; We only care about control and meta (alt key).
;;; Shift is already processed for us.
(defun make-key (val &optional (gtk-modifiers nil))
  "create a key using the gtk modifier list"
  (dolist (modifier gtk-modifiers)
    (case modifier 
      (:control-mask (incf val MOD-CONTROL-MASK))
      (:mod1-mask    (incf val MOD-META-MASK))))
  val)

#|
(defun str->key (string)
  "Parse string and return a key;. Raise an error of type
kbd-parse if the key failed to parse."
  (let ((idx 0))
    when (> (length ptr) 2)
    )
  (let* ((p (when (> (length string) 2)
              (position #\- string :from-end t :end (- (length string) 1))))
         (mods (parse-mods string (if p (1+ p) 0)))
         (keysym (stumpwm-name->keysym (subseq string (if p (1+ p) 0)))))
    (if keysym
        (apply 'make-key :keysym keysym mods)
        (signal 'kbd-parse-error :string string))))
|#



(defun on-key-press (widget event)
  "Process a key from GTK; return key structure or nil for special keys"
  (declare (ignore widget))
  (let ((val (gdk-event-key-keyval event)))
    (unless (member val *modifier-keys*)	;skip modifier keypresses
      (format t "~A~%" 
	      (key-str (make-key val (gdk-event-key-state event))))))  
  t)


(defun keystroke-setup (widget)
  ;; Initialize modifier keys - this needs to be done after keysyms are done
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






