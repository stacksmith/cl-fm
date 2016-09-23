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
As of 22-Sept-2016...

Works:
* Directory navigation by double-clicking directories;
* Also, left arrow goes up to parent - as well as ^.

Does not work
* dragging to other apps, as cl-cffi-gtk is broken. See [this issue](https://github.com/crategus/cl-cffi-gtk/issues/44) - basically, there is no way to set selection data when someone requests it.
* Drag and drop are close, just reporting for now.

Stupid
* double-clicking files starts vlc, for now - will dispatch on type soon.
* drag and drop are a little clunky with selections.


