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
03-Feb-2020
Hmm.  I haven't had much of an opportunity to move the project forward in some years now.  The main issue is that I discovered that CL-CFFI-GTK (impressive in scope) is highly problematic in implementation.  There seems to be a major misunderstanding of lightweight GTK foreign structures and their scope; the library creates ridiculous amount of temporaries in places where it is entirely unnecessary.  This happens at so many levels that I just gave up at some point.  I suppose no one cares much about megabytes upon megabytes of crap that needs to be immediately GC'd, but I am thoroughly disgusted.
At the time I was also stuck with CL-CFFI-GTK's bugs having to do with copying and pasting...
CL-MF is actually somewhat usable and I've used it for a few years in a very limited context as it allows color-coding files easily, something few fm's do.  It does not mutate anything (other than storing color attributes in metadata); copy/move/delete is not implemented.
Amazingly, after all these years, there is still no simple GUI solution (and I've grown to despise all guis anyway)..

02-Oct-2016

Made some fixes with handling of filenames.  It's still a mess and should be moved to a separate file, perhaps.  But for now it works.  The root directory is kept as a namestring (truename), so the final slash is there.  Filenames are kept in the model, and therefore have to be namestrings.  There are a few places that merge and manipulate names (activation, going up in hierarchy, etc).

Added folder icons and removed final slash in folder names in model - truename does the trick.

01-Oct-2016

Added `<F5>` reload binding.

Added a rudimentary in-place renamer.  It works, but due to internal issues with directory names ending with a slash, directory renaming is wacky.  It works only when no final slash is added in edit, and F5 must be pressed afterwards.  I will rework internal storage issues tomorrow.


| KEY | BINDING |
|-----|---------|
| `<F2>` | Edit selected file (single selection only) |
| `<F5>` | Reload current directory |
| `<LEFT>` | Open parent directory |
| `^` | Open parent directory |
| `<C-0>` | Set Q to 0 (default) |
| `<C-1>` | Set Q to 1 (red) |
| `<C-2>` | Set Q to 2  |
| `<C-3>` | Set Q to 3  |
| `<C-4>` | Set Q to 4 |
| `<C-5>` | Set Q to 5 (default) |
| `<RET>` | Activate: for folders, open here; for files, open with external application (for now VLC)

 

---
28-Sept-2016

Added in-place editing of filenames with F2.  As with everything else, no changes are made to the filesystem - file-actions.lisp is just reporting requests. Setting q will write to xattr of the file.

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

---
23-Sept-2016
* added color q tagging with `<C-0>` to `<C-5>`. This is an early test, but I've been wanting to color-tag files since early Macintosh days.  It seems there is no file manager that does it, much less one that does it easily.  So, `<C-0>` clears color tagging, and `<C-1>` to `<C-5>` let you set a color between green and red for quality or whatever.  For now, I am just colorizing the size field as color background breaks the stripy splendor of the tree control.  See the screenshot...
* `<UP>` and `<DOWN>` arrows are bound to eli's instant keymap, with a function that just returns nil so GTK can take over the scrolling.
* drag and drop is almost ready for action - try dragging files around and see the textual representation of the intended action.  Seems to be correct for copy, move, etc and also outputs file lists.

---
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


