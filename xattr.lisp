(in-package :cl-fm)
;;------------------------------------------------------------
;; Extended Attributes
;;
;; cl-fm stores the following date in extended file attributes:
;; key "q" contains a digit 0-9 for a user quality rating.
;; the quality maps to foreground colors in the display.
;; files with no q show up as black on white?
(define-foreign-library libattr
  (:unix (:or "libattr.so.1" "libattr.so")))
(use-foreign-library libattr)
;; BUG the first access seems to return success/0 sometimes?
(defun q-get (path)
  "retreive q attr given a path or a string, or nil"
  (format t ":~A~%" path)
  (with-foreign-pointer (buf 257)
    (with-foreign-object (size :int)
      (setf (mem-aref size :int) -1)
      (let ((res (foreign-funcall
		  "attr_get" 
		  :string (namestring path)
		  :string "q"
		  :pointer buf
		  :pointer size
		  :int 0
		  :int ;return
		  )))
	(and (zerop res)
	     (- (mem-aref buf :char) 48))))))

;;TODO: fix to smaller buffer
(defun q-set (value path)
  "set q attr given a path or string"
  (foreign-funcall "attr_set" 
		   :string (namestring path)
		   :string "q"
		   :string (format nil "~A" value) 
		   :int 1
		   :int 0
		   :int))

