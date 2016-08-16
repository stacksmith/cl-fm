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
(defun list-directory (dir)
  (loop for path in (cl-fad:list-directory dir)
     unless (cl-fad:directory-exists-p path)
       collect path))

(defun load-flist (&key (wd "/media/stacksmith/TEMP/Trash") (recurse nil))
  "load a file list, recuring if needed"
  (if recurse
      (walk-directory wd)
      (list-directory wd)))

(defparameter *input* nil)
(defparameter *wd* nil)
(defparameter *selected* nil)
(defun iload (&key (in *input*) (sub nil))
  (setf *wd* in
   *input* (load-flist :wd in  :recurse sub)))
(defun iselect (&key (regex ".*")))
