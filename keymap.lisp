(in-package :cl-fm)

;;; Key bindings are stored in a tree of hashtables of keys and commands.




(defun define-key (map key command)
    (let ((binding (find key (keymap-bindings map) :key 'binding-key :test 'equalp)))))
