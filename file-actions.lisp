(in-package :cl-fm)



(defun file-action (action files)
  (cond
    ((string= action "COPY") (format t "COPY ~A" files))
    ((string= action "MOVE") (format t "MOVE ~A" files))
    ((string= action "LINK") (format t "LINK ~A" files))
    ((string= action "ASK" ) (format t "ASK  ~A" files))
    ((string= action "RENAME") (format t "RENAME ~A" files))
    ))
