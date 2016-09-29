### CL-FM

## Overview

cl-fm is a gtk-based file manager, written in Common Lisp.  It features
- mouse operations
- emacs-like commands
- color-coding and tagging of files and directories
- hackability in Lisp.

![screenshot](/resources/screenshot.png?raw=true)

Work in progress.  Many unimplemented features.  Changes often.  Not ready for prime time.

## License

BSD license

## Depends on

- cl-cffi-gtk
- [gtk-emacs-like-input](https://github.com/stacksmith/gtk-emacs-like-input)

## Quickstart
Start by cloning this repo and gtk-emacs-like-input to a place where quicklisp will see it.

    (ql:quickload :cl-fm)
	(in-package :cl-fm)
	(test :dir "...") ;put your test directory here - but be careful and use a scrap dir.
	
## Status
28-Sept-2016
Added in-place editing of filenames with F2.  As with everything else, no changes are made to the filesystem - file-actions.lisp is just reporting requests.

Here is what works:

| KEY | BINDING |
|-----|---------|
| `<F2>` | Edit selected file (single selection only) |
| `<LEFT>` | Open parent directory |
| `^` | Open parent directory |
| `<C-0>` | Set Q to 0 (default) |
| `<C-1>` | Set Q to 1 (red) |
| `<C-2>` | Set Q to 2  |
| `<C-3>` | Set Q to 3  |
| `<C-4>` | Set Q to 4 |
| `<C-5>` | Set Q to 5 (default) |
| `<RET>` | Activate: for folders, open here; for files, open with external application (for now VLC)

Double-click is also activate.

Also, drag and drop (except for dragging out, as mentioned elsewhere)


23-Sept-2016
* added color q tagging with `<C-0>` to `<C-5>`. This is an early test, but I've been wanting to color-tag files since early Macintosh days.  It seems there is no file manager that does it, much less one that does it easily.  So, `<C-0>` clears color tagging, and `<C-1>` to `<C-5>` let you set a color between green and red for quality or whatever.  For now, I am just colorizing the size field as color background breaks the stripy splendor of the tree control.  See the screenshot...
* `<UP>` and `<DOWN>` arrows are bound to eli's instant keymap, with a function that just returns nil so GTK can take over the scrolling.
* drag and drop is almost ready for action - try dragging files around and see the textual representation of the intended action.  Seems to be correct for copy, move, etc and also outputs file lists.


22-Sept-2016...
Works:
* Directory navigation by double-clicking directories;
* Also, left arrow goes up to parent - as well as ^.

Does not work
* dragging to other apps, as cl-cffi-gtk is broken. See [this issue](https://github.com/crategus/cl-cffi-gtk/issues/44) - basically, there is no way to set selection data when someone requests it.
* Drag and drop are close, just reporting for now.

Stupid
* double-clicking files starts vlc, for now - will dispatch on type soon.
* drag and drop are a little clunky with selections.


