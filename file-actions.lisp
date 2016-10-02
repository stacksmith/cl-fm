(in-package :cl-fm)



(defun file-action (action arg1 arg2)
  (cond
    ((string= action "COPY") (format t "COPY ~A ~A" arg1 arg2))
    ((string= action "MOVE") (format t "MOVE ~A ~A"  arg1 arg2))
    ((string= action "LINK") (format t "LINK ~A ~A"  arg1 arg2))
    ((string= action "ASK" ) (format t "ASK  ~A ~A"  arg1 arg2))
    ((string= action "RENAME") (format t "RENAME ~A ~A"  arg1 arg2))
    ))

;; TODO: error protect
(defun action-rename-one (fb src dst)
  "rename one file or dir.  Do not end in slash for now."
  (with-slots (path) fb
    (unless (uiop:directory-pathname-p dst)
      (let ((psrc (merge-pathnames src path))
	    (pdst (merge-pathnames dst path))))
      (format t "ACTION-RENAME-ONE: from [~A] to [~A]~%" psrc pdst)
      (rename-file psrc pdst))))
