(in-package :cl-fm)

;;; Key bindings are stored in a tree of hashtables of keys and values.
;;; key: a key with modifiers, or a string
;;; value: another map or a symbol of function to execute (not nil!)
;;;
;;; A full binding like "M-x C-Q test" corresponds to a keymap containing
;;; M-x, which in turn contains another keymap containing C-Q, which has
;;; "test" bound to some function.


(defparameter *keymap-top* (make-hash-table :test #'equal))

(defun bind-find-or-create (map key)
  "find or create a keymap in map corresponding to key"
  (or (gethash key map) ;if exists, return it
      (make-hash-table :test #'equal)))

(defun bind-command (map key symbol)
  "bind a key or a string to a function symbol "
  
  (setf (gethash key map) symbol))

(defun keyname->gtkkey (string index)
  "convert keyname from string at index to gtkkey"
  (or (gtkkey-name->gtkkey (subseq string index)) ; do
      (signal 'kbd-parse-error :string string) ; or die
      ))

;;; a key-pair is a string 3+ long, with the first character indicating a
;;; modifier (CMASHh), second being a -, and rest- a string convertible
;;; to a key.
(defun parse-key-pair (string index remaining key)
  "parse emacs-command string at index updating key, returning 4 values"
  (case remaining
    (1 (incf key (char-code (char string index)));last char must be char
       (incf index 1)
       (decf remaining 1)) 
    (t (if (eq #\- (char string (1+ index))) ; command formed as "?-..."
	   (progn ; attempt to set modifier
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
	   (progn ; not a -, remainder must be convertible to a key
	     (incf key (keyname->gtkkey string index))
	     (incf index remaining) ;done here
	     (setf remaining 0)))))
  (values string index remaining key))
;;; A key-tuple is a string containing no spaces corresponding to a keystroke
;;; with 0 or more modifiers in the form of one of (CMASHh) characters followed
;;; by a -.  The final element must be found to be a keyname.
(defun parse-key-tuple (string)
  "parse emacs-command string, returning key"
  (let ((index 0) (remaining (length string)) (key 0))
    (loop while (> remaining 0) do
	 ;(format t "~A ~A ~A ~A~%" string index remaining key)
	 (multiple-value-setq (string index remaining key)
	   (parse-key-pair string index remaining key)))
    key))

;;; A command-sequence is a string containing multiple key-tuples, ending with
;;; a string that represents a function
(defun parse-command-sequence (string)
  (let ((items (split-string string)))
    (loop for item in items
       while (cdr item) do
	 (format t "~A~%" item))))


(defun bind (string command)
   (let ((index 0) (remaining (length string)) (key 0))
    (loop while (> remaining 0) do
	 (format t "~A ~A ~A ~A~%" string index remaining key)
	 (multiple-value-setq (string index remaining key)
	   (parse-key-pair string index remaining key)))
    key)

  )
