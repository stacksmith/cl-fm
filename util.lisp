(in-package :cl-fm)
;; Filename structure holds original path and transformed name...
(defstruct fentry path size)

(defun walk-directory (dir)
  "walk directory and subdirs, return a list of all files" 
  (let ((list nil))
    (cl-fad:walk-directory 
     dir
     #'(lambda (fname) (push fname list))
     :directories nil)
    list))


(defun load-flist (&key (wd "/media/stacksmith/TEMP/Trash") (recurse nil))
  "load a file list, recuring if needed"
  (if recurse
      (walk-directory wd)
      (cl-fad:list-directory wd)))
