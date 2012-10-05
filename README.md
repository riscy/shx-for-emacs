shx-for-emacs
=============

shx or "shell-extras" extends comint-mode:

- It parses simple markup in the output stream
  - Plots/graphics can be automatically embedded in the shell
  - Links become clickable and keyboard accessible
  - An Emacs-wide alert can tell the user a task is done
- It adds several command-line functions which plug into Emacs
  - Examine a plot of some data file without changing context
  - Display a (clickable) Google Maps image of a given location
  - Adding new functions is easy (for an example see below)
- It automatically splits the screen when you page up and down
  - This lets you keep context when referring to earlier output

This version has been tested with Emacs 24.0.94 on Mac OS X.


Warning!
========

This software makes Emacs react automatically to the text displayed
in a buffer.  Since this text might be beyond your direct control,
Emacs will be too.  I've tried to ensure shx triggers safely (see
`shx-safe-to-trigger') but shx comes without any warranty.


Installation
============

1. Move shx.el to a directory in your 'load-path'.  Alternatively
   add shx.el's directory to your 'load-path' by adding a line like
   the following to your .emacs:

  (add-to-list 'load-path "~/path/to/elisp/")

2. Next add this line to your .emacs:

  (require 'shx)

If you want shx to run in any comint-mode buffer, add this too:

  (add-hook 'comint-mode-hook 'shx-activate)

That's it!


Requirements
============

No additional software is necessary, but the graphical functions
(like plotting) use these programs:

- convert (i.e., ImageMagick, used to scale images to size)
- gnuplot (for all plotting functions)

(I've been working to make this list shorter.)

The variables 'shx-convert-cmd' and 'shx-gnuplot-cmd' can be
customized as needed (or edited in shx.el) to point to these
binaries.


Quick-Start
===========

1. Finish the installation (as above).
2. Type M-x shx (enter) to begin a shell session using shx
3. Type :man ls
4. Type delay 2 echo -e "\n##done(TEST)"
5. Type :help
6. Type :test (hopefully you see nothing but a success message)
7. Try to page up, enter a command, then page back down

Detailed help can be found in the next few sections.


shx Input Commands
==================

shx's support for input commands written in elisp gives it a lot of
the same advantages as 'eshell'.

Everything you need to know about shx's input commands can be found
in the help.  Just type :help on an empty line and press enter.

These special commands are executed asynchronously of the
underlying shell process because Emacs just intercepts them.  So
you can type ":man gcc" even while gcc is busy compiling and a
window with the gcc man page will come up within Emacs.

The commands that get intercepted by shx will have the
'shx-highlights' face, whereas commands which were not intercepted
will have the default 'comint-highlight-input' face.

Many shx commands are for displaying graphics such as plots in a
shell buffer.  These require ImageMagick and gnuplot to be
installed.  Others invoke built-in Emacs functionality, like :man,
:edit, :grep, :delay.

Users can write new commands by defining a single-argument function
named shx-COMMAND, where COMMAND (which must be capitalized) is
what the user would type to invoke it.  For example if you put this
in your .emacs:

  (defun shx-BREAK (arg) (insert "Break!") (shx-send-break))

... a user can type :break to send a break straight through.  See
'shx-DIFF', 'shx-GREP' for examples.

If you write a new command that you think might be useful to
others, send it along to me and hopefully I can include it.


shx Command Triggers
====================

Triggers can be used to enhance command-line applications.  They
are invoked by a simple markup language: just have the application
output something like the following on a line by itself:

  ##COMMAND(ARGUMENT)

For example if shx sees the following line:

  ##view(mountains.png)

... then mountains.png will be displayed in the shell, scaled to fit.

You can control how much vertical space any image occupies by
customizing the variable 'shx-imgsize', or by executing:

  (setq shx-imgsize 300)


shx Scrolling
=============

shx supports splitting of a shell window on paging up and down.
When you page up, the frame is split in two with a larger
"scrolling frame" on top and a smaller "input frame" preserved on
the bottom.  This lets you enter text at the prompt (in the input
frame) and monitor new input while consulting previous output (in
the scrolling frame) uninterrupted.

You can change the size of the input frame to something else:

  (setq shx-split-rows 15)

Or to disable this feature:

  (setq shx-split-rows 0)

n.b., function 'shx-tests' will fail if this feature is disabled.


shx Keys
========

- Recognized URLs are turned into mouse/keyboard accessible links
  (C-c b) and a history of previous links is maintained.

- C-c C-c sends the break character (C-c).

- C-c C-k sends a SIGKIL to the current subjob (what C-c C-c did
- before).

- When the prompt is a ":" (such as when reading through a man
  page), leading spaces and 'q's are sent straight to the process
  rather than being inserted into the buffer.

You can change the 'shx-prefix' (the prefix you type at the prompt
before shx commands) from (say) ":" to "# " as follows:

  (setq shx-prefix "# ")

in which case you would, for example, type "# help" to access the
help.

Or you can set the prefix to nothing all:

  (setq shx-prefix "")


Priorities
==========

Testing, documentation
