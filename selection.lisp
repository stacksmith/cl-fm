(in-package :cl-fm)

(defun fb-selected-count (fb)
  "return t if multiple files selected"
  (gtk-tree-selection-count-selected-rows
   (gtk-tree-view-get-selection (filebox-widget fb)))
)

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
     (funcall func (fb-full-namestring fb (fb-model-value COL-NAME))))))

#|
  (let ((count 0))
    (gtk-tree-selection-selected-foreach
     (gtk-tree-view-get-selection (filebox-widget fb))
     (lambda (model path iter)
       (declare (ignore model path iter))
       (incf count)))
    count)
|#


