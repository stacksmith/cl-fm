(in-package :cl-fm)

(defun on-edited (fb renderer path new-text)
  (declare (ignore renderer path))
  ;; cl-cffi-gtk bug: path is bad
    
  (format t "EDITED: from [~A] to [~A]~%" (car (fb-selected-paths fb))
	  (fb-merge-path fb new-text))
  (format t "FILEBOX-PATH ~A~%" (filebox-path fb))
  (eli::suspend (filebox-eli fb) nil))		;re-enable eli
  

(defun on-editing-started (fb renderer editable path)
  )

(defun on-editing-canceled (fb renderer)
  (eli::suspend (filebox-eli fb) nil)		;re-enable eli
  )

(defun init-name-editing (fb)
  "initialize name editing system"
     ;; for in-place editing, bind "edited" message from name renderer
    (fb-signal-connect (filebox-renderer-name fb) "edited" on-edited (renderer path new-text))
    (fb-signal-connect (filebox-renderer-name fb) "editing-started" on-editing-started (renderer editable path))
    (fb-signal-connect (filebox-renderer-name fb) "editing-canceled" on-editing-canceled (renderer)))


(defun name-edit (eli)
  "invoke an editor on the filename, if possible"
  (let* ((fb (eli-payload eli))
	 (tv (filebox-widget fb))
	 (sel (gtk-tree-view-get-selection tv))
	 (paths (gtk-tree-selection-get-selected-rows sel))
	 (renderer (filebox-renderer-name fb)))
    (when (and paths (null (cdr paths))) ;only for single selection
      ;; allow editing, start edit and immediately disallow editing to make sure
      ;; standard treeview activation does not edit.
   (format t "EDITING: from [~A] ~%"
					;(model-path->name (filebox-store fb) path)
	   (car paths)
	  )    
      (eli::suspend eli t) ;resumed in on-edited and on-editing-canceled
      (setf (gtk-cell-renderer-text-editable renderer) t) ;allow editing
      (gtk-tree-view-set-cursor tv (car paths) :focus-column (filebox-column-name fb) :start-editing t)
      (setf (gtk-cell-renderer-text-editable renderer) nil))))
