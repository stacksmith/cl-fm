(in-package :cl-fm)

(defun fb-merge-path (fb subpath)
  (merge-pathnames subpath (pathname (filebox-path fb))))

(defun fb-selected-paths (fb)
  (let ((basepath (pathname (filebox-path fb))))
    (with-slots (store selection) fb
      (mapcar #'(lambda (subpath)
		  (merge-pathnames (model-path->name store subpath)
				   basepath))
	      (gtk-tree-selection-get-selected-rows selection)))))
