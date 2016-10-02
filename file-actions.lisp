(in-package :cl-fm)



(defun file-action (action arg1 arg2)
  (cond
    ((string= action "COPY") (format t "COPY ~A ~A" arg1 arg2))
    ((string= action "MOVE") (format t "MOVE ~A ~A"  arg1 arg2))
    ((string= action "LINK") (format t "LINK ~A ~A"  arg1 arg2))
    ((string= action "ASK" ) (format t "ASK  ~A ~A"  arg1 arg2))
    ((string= action "RENAME") (format t "RENAME ~A ~A"  arg1 arg2))
    ))

(defun action-rename-one (src dst)
  
  (rename-file src dst))
