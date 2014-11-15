# What is Origami?

A text folding minor mode for Emacs.

With this minor mode enabled, you can collapse and expand regions of
text.

The actual buffer contents are never changed in any way. This works by
using overlays to affect how the buffer is presented. This also means
that all of your usual editing commands should work with folded
regions. For example killing and yanking folded text works as you
would expect.

There are many commands provided to make expanding and collapsing text
convenient.

# What does it look like?

![origami](http://www.gregsexton.org/images/origami-screen.png)

# How do I install it?

Firstly, origami requires the following dependencies:

* https://github.com/magnars/dash.el
* https://github.com/magnars/s.el

You should install these anyway, they make working with elisp much
more comfortable.

Drop this package somewhere on your load-path or

    (add-to-list 'load-path (expand-file-name "/path/to/origami.el/"))

Then

    (require 'origami)

In a buffer run `M-x origami-mode`, and start experimenting with any
of the supplied origami interactive functions. I recommend binding
these to keys of your choice in the `origami-mode-map`.

This has been tested on Emacs 24.3 and 24.4.

# What can it do?

Origami works by parsing the buffer to determine a fold structure.
(Currently there is only support for determining the fold structure
using a parser.)

The following commands are supplied to manipulate folds in the buffer:

* origami-open-node -- Open a fold node.

* origami-open-node-recursively -- Open a fold node and all of its
  children.

* origami-show-node -- Like origami-open-node but also opens parent
  fold nodes recursively so as to ensure the position where point is
  is visible.

* origami-close-node -- Close a fold node.

* origami-close-node-recursively -- Close a fold node and all of its
  children.

* origami-toggle-node -- Toggle open or closed a fold node.

* origami-forward-toggle-node -- Search forward on this line for a
  node and toggle it open or closed. This makes toggling nodes much
  more convenient.

* origami-recursively-toggle-node -- Acts like org-mode header
  collapsing. Cycle a fold between open, recursively open, closed.

* origami-open-all-nodes -- Open every fold in the buffer.

* origami-close-all-nodes -- Close every fold in the buffer.

* origami-show-only-node -- Close everything but the folds necessary
  to see the point. Very useful for concentrating on an area of code.

* origami-previous-fold -- Move to the previous fold.

* origami-next-fold -- Move to the next fold.

* origami-undo -- Undo the last folding operation.

* origami-redo -- Redo the last undone folding operation.

* origami-reset -- Remove all folds from the buffer and reset all
  origami state. Useful if origami messes up!

# Does it support my favourite major-mode?

Probably not. Currently out of the box support is provided for:

* C
* C++
* Clojure
* Java
* Perl
* elisp

It should be trivial to add support for any language that uses braces
to delimit blocks. Just add to `origami-parser-alist` something like:
`(mode-name . origami-c-style-parser)`. Adding support for another
lisp dialect should be almost as simple.

I'm happy to work on parsers for other languages if enough interest is
expressed.

It should be fairly easy to write a parser. An origami parser is a
function that takes a 'create function' and returns a function taking
the string to be parsed. The returned function should return a list of
fold nodes. Fold nodes are created using the passed-in create
function. Best to use an example:

     (defun my-amazing-parser (create)
       (lambda (content)
         (list (funcall create beginning-of-the-fold-node-point-position
                               end-of-the-fold-node-point-position
                               offset  ; this allows you to show some of the start of the folded text
                               child-nodes))))