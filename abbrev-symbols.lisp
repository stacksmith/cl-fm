;;;
;;;
(in-package :cl-fm)

(defun abbrev-symbols (package old-prefix new-prefix)
  (let ((old-prefix-length (length old-prefix)))
    (do-external-symbols (sym (find-package package))
      (when (fboundp sym) ;functions only
	(let ((sym-name (symbol-name sym)))
	  (when-let (match (search old-prefix sym-name))
	    (when (zerop match)
	      (let* ((abbrev-sym-name
		      (concatenate 'string new-prefix
				  (subseq sym-name old-prefix-length))))
		(if (find-symbol abbrev-sym-name package)
		    (format t "Symbol ~A already exists" abbrev-sym-name)
		    (let ((abbrev-sym (intern abbrev-sym-name package)))
		      (format t "Interned ~A~%" abbrev-sym)
		      (export abbrev-sym package)
		      (setf (symbol-function abbrev-sym) (symbol-function sym))
		      (format t "created ~A~%" abbrev-sym)))))))))))

(setf gtvc-get-title 1)


(defun process-symbols (package function)
  (do-external-symbols (sym (find-package package))
    (funcall function sym)))
