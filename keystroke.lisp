(in-package :cl-fm)
;;; Keyboard
;;;
;;; keysym is the hardware keycode from GTK
;;;

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
	       (gtkkey->gtkkey-name (key-val key))))
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

(defun str->key-prim (string index remaining key)
  "parse emacs-command string at index updating key, returning 4 values"
  ;; remaining must be >0!
  (case remaining
    (1 (incf key (char-code (char string index)));last char is the char
       (incf index 1)
       (decf remaining 1)) 
    (t (if (eq #\- (char string (1+ index))) ; command formed as "?-..."
	   (progn
	     (case (char string index) ;dispatch on the letter preceding #\-
	       (#\C (incf key mod-control-mask))
	       (#\M (incf key mod-meta-mask))
	       (#\A (incf key mod-alt-mask))
	       (#\S (incf key mod-shift-mask))
	       (#\s (incf key mod-super-mask))
	       (#\H (incf key mod-hyper-mask))
	       (t (signal 'kbd-parse-error :string string)))
	     (incf index 2)
	     (decf remaining 2))
	   (signal 'kbd-parse-error :string string)))) ;not a -, malformed
  (values string index remaining key))

    
(defun str->key (string)
  "parse emacs-command string, returning key"

  (let ((index 0) (remaining (length string)) (key 0))
    (loop while (> remaining 0) do
	 (format t "~A ~A ~A ~A~%" string index remaining key)
	 (multiple-value-setq (string index remaining key)
	   (str->key-prim string index remaining key)))
    key))

 

(defun on-key-press (widget event)
  "Process a key from GTK; return key structure or nil for special keys"
  (declare (ignore widget))
  (let ((gtkkey (gdk-event-key-keyval event)))
    ;;    (format t "...~A~%" gtkkey)
    (unless (modifier-p gtkkey)	;skip modifier keypresses
      (format t "~A~%" 
	      (key-str (make-key gtkkey (gdk-event-key-state event))))))  
  t)


(defun keystroke-setup (widget)     ;; wiring
    (g-signal-connect widget "key-press-event" 'on-key-press)
   ; (g-signal-connect widget "key-release-event" 'on-key-release)
)






