(in-package :cl-fm)

(defun fb-selected-count (fb)
  "return t if multiple files selected"
  (let ((count 0))
    (gtk-tree-selection-selected-foreach
     (gtk-tree-view-get-selection (filebox-widget fb))
     (lambda (model path iter)
       (declare (ignore model path iter))
       (incf count)))
    count))

(defun foreach-selected-row (fb func)
  "func is (lambda (model path iterator).."
  (gtk-tree-selection-selected-foreach
   (gtk-tree-view-get-selection (filebox-widget fb))
   func))

(defun foreach-selected-pathstring (fb func)
  "func is (lambda (pathstring).."
  (gtk-tree-selection-selected-foreach
   (gtk-tree-view-get-selection (filebox-widget fb))
   (lambda (model path iter)
     (declare (ignore path))
     (funcall func (fb-pathname fb (fb-model-value COL-NAME))))))
   
 

