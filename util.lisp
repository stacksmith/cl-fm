(in-package :cl-fm)
;; Filename structure holds original path and transformed name...
(defstruct fentry path size)
#|
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
|#
(defun split-seq (seq separators &key (test #'char=) (default-value '("")))
  "split a sequence into sub sequences given the list of seperators."
  (let ((seps separators))
    (labels ((sep (c)
               (find c seps :test test)))
      (or (loop for i = (position-if (complement #'sep) seq)
                then (position-if (complement #'sep) seq :start j)
                as j = (position-if #'sep seq :start (or i 0))
                while i
                collect (subseq seq i j)
                while j)
          ;; the empty seq causes the above to return NIL, so help
          ;; it out a little.
          default-value))))

(defun split-string (string &optional (separators " "))
  "Splits STRING into substrings at separator characters, eliminating those"
  (split-seq string separators))

