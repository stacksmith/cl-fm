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

(defun q-get (path)
  "retreive q attr given a path or a string"
  (let ((v nil))
    (with-foreign-pointer (buf 257)
      (with-foreign-object (size :int)
	(let ((res (foreign-funcall
		    "attr_get" 
		    :string (namestring path)
		    :string "q"
		    :pointer buf
		    :pointer size
		    :int 0
		    :int				;return
		    )))
	  (if (= 0 res)
	      (setf v (- (mem-aref buf :char) 48)) 
	      (setf v nil)))))
        v))
;;TODO: fix to smaller buffer
(defun q-set (value path)
  "set q attr given a path or string"
  (let ((q (with-output-to-string (str) (format str "~A" value)) ))
    (foreign-funcall "attr_set" 
		     :string (namestring path)
		     :string "q"
		     :string q
		     :int 1
		     :int 0
		     :int ;return
		     )))

