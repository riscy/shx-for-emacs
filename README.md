shx-for-emacs
=============

`shx' or "shell-extras" extends comint-mode:

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

This version tested with Emacs 24.4.1 ...

(shx comes without any warranty; use it however you like)


Installation
============

1. Move shx.el to a directory in your `load-path'.  If you don't
   know what your load-path is type Or, add
   shx.el's directory to your `load-path' by adding a line like
   this to your .emacs:
(add-to-list 'load-path "~/path/to/elisp/")

2. Next add this line to your .emacs:
(require 'shx)

If you want shx to run in ANY comint-mode buffer, add this too:
(add-hook 'comint-mode-hook 'shx-activate)

That's it!


Quick-Start
===========

1. Finish the installation (as above).
2. Type M-x shx (enter) to begin a shell session using shx
3. Type :man ls
4. Type :delay 2 echo -e "\n##done(TEST)"
5. Type :help
6. Type :test (hopefully you see nothing but a success message)
7. Try to page up, enter a command, then page back down

Detailed help can be found in the next few sections.


Requirements
============

The graphical functions (like plotting) currently use:
- convert (i.e., ImageMagick, used to scale images to size)
- gnuplot (for all plotting functions)

You can edit shx.el (near the top) to make `shx-convert-cmd' and
`shx-gnuplot-cmd' point to these binaries.


shx Input Commands
==================

Everything you need to know about shx's input commands can be found
in the help.  Just type :help on an empty line and press enter.

Many shx commands are for displaying graphics such as plots in a
shell buffer.  These require ImageMagick and gnuplot to be
installed.  Others invoke built-in Emacs functionality, like :man,
:edit, :grep, :delay.  Nothing extra is needed in this case.

You can change the prefix you type at the prompt before shx
commands from ":" to "# " by putting this line in your .emacs,
(setq shx-prefix "# ")
in which case you would type "# help" to access the help.  Or you
can set the prefix to nothing all:
(setq shx-prefix "")
in which case you would type "help" to access the help.


shx Input Commands -- roll your own
===================================

Skip this section unless you're interested in how shx works and
maybe in writing your own input commands.

shx's support for input commands written in elisp gives it a lot of
the same advantages as `eshell'.  Users can write new commands by
defining a single-argument elisp function named shx-COMMAND, where
COMMAND (which must be capitalized) is what the user would type to
invoke it.  For example if you put this in your .emacs:

(defun shx-BREAK (arg) (insert "Break!") (shx-send-break))

... a user can type :break to send a break straight through.  See
`shx-DIFF', `shx-GREP' for examples.

If you write a new command that you think might be useful to
others, send it along to me and hopefully I can include it.


shx Command Triggers
====================

"Triggers" can enhance command-line applications -- just have your
application output `##COMMAND(ARGUMENT)' on a line by itself (where
COMMAND is one of shx's input commands) and the associated command
will be invoked with the given argument.  For example, if shx sees:
##view(mountains.png)
then mountains.png will be displayed in the shell, scaled to fit.
Useful for showing progress indicators, graphs, etc.

You can control how much vertical space any image occupies by
customizing the variable `shx-imgsize', or by executing:
(setq shx-imgsize 300)


shx Scrolling
=============

By default, shx splits the window on paging up and down.  When you
page up, the frame is split in two with a larger "scrolling frame"
on top and a smaller "input frame" preserved on the bottom.  This
lets you enter text at the prompt (in the input frame) and monitor
new input while consulting previous output (in the scrolling frame)
uninterrupted.

You can change the size of the input frame to something else:
(setq shx-split-rows 10)

Or disable this feature entirely:
(setq shx-split-rows 0)

NOTE: typing :test will fail if you disable this feature.


shx Keys
========

- C-c C-c sends a break character (C-c).

- C-c C-z sends a stop character (C-z).

- C-c C-k sends a SIGKIL to the subjob (what C-c C-c did before).

- Recognizable URLs are turned into mouse/keyboard accessible links
  (C-c b) and a history of previous links is maintained.

- When the prompt is a ":" (such as when reading through a man
  page), leading spaces and 'q's are sent straight to the process
  rather than being inserted into the buffer.
