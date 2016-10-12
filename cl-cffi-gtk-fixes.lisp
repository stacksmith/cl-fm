(in-package :gdk)
;;; As of cl-cffi-gtk-20160208-git, these are not implemented...

;; TODO: check for symbols, do not redefine if exist...

(defcfun ("gdk_window_get_cursor" gdk-window-get-cursor)
    (g-object gdk-cursor)
  (window (g-object gdk-window)))
(export 'gdk-window-get-cursor)

(defcfun ("gdk_window_set_cursor" gdk-window-set-cursor) :void
  (window (g-object gdk-window))
  (cursor (g-object gdk-cursor)))
(export 'gdk-window-set-cursor)

(defcfun ("gdk_drag_context_get_suggested_action"
	  gdk-drag-context-get-suggested-action) gdk-drag-action 
  (context (g-object gdk-drag-context)))
(export 'gdk-drag-context-get-suggested-action)

(in-package :cl-fm)
(eval-when (:compile-toplevel)
  (abbrev-symbols :gtk "GTK-TREE-VIEW" "GTV"))
