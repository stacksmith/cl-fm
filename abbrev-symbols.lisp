;;;
;;;
(in-package :cl-fm)
;; create abbreviates function symbols in a package.
;;
;; For instance, make synonym symbols for all functions starting with
;; gtk-tree-view-...; our synonyms start with gtv- like this
;;
;; (eval-when (:compile-toplevel)
;;   (abbrev-symbols 'gtk "GTK-TREE-VIEW" "GTV"))
;;
;; Notes:
;; - this only works on functions;
;; - setf expanders are not affected (making it not too useful)
;; - classes and other namespaces are not affected
;;
;; This is hardly worth it, but was interesting to look at.  Perhaps
;; another project...

(defun abbrev-symbols (package old-prefix new-prefix)
  "Create synonyms starting with 'new-prefix' for function symbols 
starting with 'old-prefix' in package"
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


(defun process-symbols (package function)
  "not used, apply a function to all symbols in a package"
  (do-external-symbols (sym (find-package package))
    (funcall function sym)))
