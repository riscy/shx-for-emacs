;;; shx.el --- shell-extras, extras for the (comint-mode) shell
;;
;; Copyright (C) 2012 Chris Rayner
;;
;; Author: rayner AT cs DOT ualberta DOT ca
;; Created: 23 May 2011
;; Version: alpha
;; Keywords: comint-mode, shell-mode
;; URL: http://www.cs.ualberta.ca/~rayner/
;; Git: https://github.com/riscy/shx-for-emacs
;;
;;
;;; Commentary:
;;
;; `shx', "shell-extras", extends comint-mode in a couple small ways.
;; It displays small plots and graphics, changes how page up/down work,
;; and intercepts commands entered at the shell to call elisp functions.
;;
;; This version has been tested with Emacs 24.0.94 on Mac OS X.
;;
;; Installation
;; ============
;;
;; Put shx.el somewhere in your `load-path' and add this line to your ~/.emacs:
;;
;;; (require 'shx)
;;
;; If you want shx to run in ANY comint-mode buffer, add this line too:
;;
;;; (add-hook 'comint-mode-hook 'shx-activate)
;;
;; Full graphical functionality requires the following:
;; - convert (i.e., ImageMagick, for scaling images)
;; - gnuplot (for all plotting functions)
;; - wget    (for pulling image files from google maps)
;;
;; The variables `shx-convert-cmd' `shx-gnuplot-cmd' and `shx-wget-cmd'
;; can be customized as necessary to point to these executables.
;;
;;
;; Quick-Start
;; ===========
;;
;; 1. Finish the installation (as above).
;; 2. Type M-x shx <enter> to begin a shell session using shx
;; 3. Type :man ls
;; 4. Type echo -e "\n##done()"
;; 5. Type :help
;; 6. Type :test  (hopefully you see nothing but a success message)
;; 7. Try to page up, run a command at the prompt, then page back down
;;
;; Detailed help can be found in the next few sections.
;;
;;
;; shx Input Commands
;; ==================
;;
;; shx's support for input commands written in elisp give it a lot of
;; the same advantages as `eshell'.
;;
;; Everything you need to know about shx's input commands can be found
;; in the help.  Just type :help on an empty line and press enter.
;;
;; These special commands are executed asynchronously of the underlying
;; process because emacs intercepts them as soon as you hit enter.  For
;; example you can type ":man ssh" even while the underlying process is
;; busy.
;;
;; You can change `shx-prefix' from ":" to "# ",
;;
;;; (setq shx-prefix "# ")
;;
;; in which case you would, for example, type "# help" to access the
;; help.
;;
;; Or you can set the prefix to nothing all:
;;
;;; (setq shx-prefix "")
;;
;; The commands that get intercepted by shx will have the
;; `shx-highlights' face, whereas commands which were not intercepted
;; will have the default `comint-highlight-input' face.
;; 
;; Many existing commands are for displaying graphics such as plots in a
;; shell buffer.  These require ImageMagick, wget, and gnuplot to be
;; installed.  Others invoke built-in emacs functionality, like :man,
;; :edit, :grep, :delay.
;;
;; Users can write new commands by defining a single-argument function
;; of the form shx-COMMAND, where COMMAND (which must be capitalized) is
;; what the user would type to invoke it.  For example if you put this
;; in your .emacs:
;;
;;; (defun shx-BREAK (arg) (insert "Break!") (shx-send-break))
;;
;; ... a user can type :break to send <C-c> straight through.  See
;; `shx-DIFF', `shx-GREP' for examples.
;;
;; If you write a new command that you think might be useful to others,
;; send it along to me and hopefully I can include it.
;;
;;
;; shx Command Triggers
;; ====================
;;
;; Triggers can use many of the input commands (above) to enhance
;; command-line applications.  This is done by having the application
;; echo a shx command trigger on a new line by itself:
;;
;;; ##COMMAND(ARGUMENT)
;; 
;; For example if a program outputs the following line:
;;
;;; ##view(mountains.png)
;;
;; ... then mountains.png will be displayed in the shell, scaled to fit.
;;
;; You can control how large the image appears on-screen by customizing
;; the variable `shx-imgsize', or by executing:
;;
;;; (setq shx-imgsize 300)
;;
;;
;; shx Scrolling
;; =============
;;
;; shx supports splitting of a shell window on paging up and down.  When
;; you page up, the frame is split in two with a larger "scrolling
;; frame" on top and a smaller "input frame" preserved on the bottom.
;; This lets you enter text at the prompt (in the input frame) and
;; monitor new input while consulting previous output (in the scrolling
;; frame) uninterrupted.
;;
;; You can change the default size of the input frame to something else:
;;
;;; (setq shx-row-split 15)
;;
;;
;; shx Keybinding Modifications
;; ============================
;;
;; - Recognizable URLs are turned into mouse/keyboard accessible (C-c b)
;;   links and a history of previous links is maintained.
;;
;; - C-c C-c sends <C-c> to the foreground process
;;
;; - C-c C-k sends SIGKILL to the shell (what C-c C-c did before).
;;
;; - When the prompt is a ":" (such as when reading through a man page),
;;   leading spaces are sent straight through to the process rather than
;;   being inserted into the buffer, for simpler paging.
;;
;;
;; Priorities
;; ==========
;;
;; Testing


;;; Code:

(require 'comint)


;;; =====================================================================
;;;              Global variables and customization options


;; Programs used (try absolute paths if not working for you)
(defvar shx-convert-cmd "convert")
(defvar shx-gnuplot-cmd "gnuplot")
(defvar shx-wget-cmd    "wget")

;; Some other variables
(defvar shx-prefix ":")
(defvar shx-imgsize 350)
(defvar shx-row-split 15)
(defun shx-gmap-url ()
  (concat "http://maps.google.com/maps/api/staticmap"
          "?maptype=roadmap&zoom=13&sensor=false&size="
          (number-to-string shx-imgsize) "x"
          (number-to-string shx-imgsize) "&markers="))

(defun shx-get-mode-map (&optional parent)
  "Keymap used for `shx-mode'."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map parent)
    ;; ease of use:
    (define-key map (kbd "SPC") 'shx-space-maybe)
    ;; scrolling:
    (define-key map (kbd "<prior>") 'shx-scroll-up)
    (define-key map (kbd "<next>") 'shx-scroll-down)
    (define-key map (kbd "<home>") 'shx-scroll-home)
    (define-key map (kbd "<end>") 'shx-scroll-end)
    (define-key map (kbd "M-<") 'shx-scroll-home)
    (define-key map (kbd "M->") 'shx-scroll-end)
    ;; C-c user shortcuts...
    ;;;(define-key map (kbd "C-c C-o") 'shx-browse-url)
    (define-key map (kbd "C-c b") 'shx-browse-url) ; like rcirc
    (define-key map (kbd "C-c C-c") 'shx-send-break)
    (define-key map (kbd "C-c C-k") 'comint-kill-subjob)
    ;; Send a command
    (define-key map (kbd "<return>") 'shx-send-input)
    map))

(defvar shx-mode-map (shx-get-mode-map))

(defface shx-highlights
  '((((class color)) (:foreground "#FF7030")))
  "Face to highlight user input that went through shx."
  :group 'shx)


;;; =====================================================================
;;;                    Hooks and input commands


(defun shx-get-current-input ()
  "Return what's written after the prompt -- i.e., the string
between the `process-mark' and the `line-end-position'."
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (buffer-substring-no-properties pmark (line-end-position))))


(defun shx-get-current-prompt ()
  "Return raw text from start of line to current process-mark."
  (save-excursion
    (goto-char (point-max))
    (let ((inhibit-field-text-motion t))
      (buffer-substring-no-properties
       (point-at-bol)
       (process-mark (get-buffer-process (current-buffer)))))))


(defun shx-show-echo-hook (input)
  "When the user hits enter (sends input) we may have something
cached in the variable `shx-echo' to insert into the output
stream, such as some triggerable text -- see
`shx-parse-output-hook'.  This function should be in
`comint-input-filter-functions'."
  (when (not (eq shx-echo nil))
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-field-text-motion t))
       (beginning-of-line))
      (backward-char)
      (insert "\n" shx-echo))
    (set 'shx-echo nil)))


(defun shx-send-input ()
  "Send or parse the input currently written at the prompt."
  (interactive)
  (let* ((cmd (shx-get-current-input))
         (parsed (shx-parse-input-commands cmd)))
    ;; If `shx-parse-input-commands' found something to parse the input will
    ;; have been replaced with a placeholder " ".  We still send this placeholder
    ;; through to the process (so as to get a fresh prompt):
    (comint-send-input nil t)
    (when parsed
      ;; Now we need to restore the CMD the user originally wrote, and cook the
      ;; books so that CMD input appears in comint's history list.
      (save-excursion
        ;; Bookkeeping
        (comint-add-to-input-history cmd)
        ;; Delete the placeholder text
        (comint-previous-prompt 1)
        (delete-region (point) (line-end-position))
        ;; Restore CMD text
        (insert (propertize cmd
                            'front-sticky t
                            'mouse-face 'link
                            'field 'input
                            'font-lock-face 'shx-highlights
                            'help-echo
                            (concat "Parsed by shx (shx-echo " parsed ")")))))))


(defun shx-parse-input-commands (str)
  "Scan the text STR written at the prompt.  If STR starts with
shx-prefix and specifies a valid user command (e.g., :view would
specify `shx-VIEW'), replace what's written with a placeholder.
We will save a triggerable string to `shx-echo' (e.g.,
'##view(image.png)') which will later be inserted in the output
stream by `shx-show-echo-hook'."
  (interactive)
  (if (string-match (concat "^" shx-prefix "\\(\\w+\\)[\s\t]+\\(.*[^\s\t]\\)[\s\t]*") str)
      ;; If the input matches <shx-prefix><cmd> <arg>:
      (let* ((cmd (intern (format "shx-%s" (upcase (match-string 1 str))))))
        ;; If such a command exists
        (when (fboundp cmd)
          ;; clear the prompt
          (delete-region (line-beginning-position) (line-end-position))
          ;;(insert " ") ; <-- insert SOMETHING or comint forgets this line...?
          (set 'shx-echo (concat "##" (match-string 1 str) "(" (match-string 2 str) ")"))))
    ;; Otherwise if the input matches <shx-prefix><cmd>:
    (when (string-match (concat "^" shx-prefix "\\(\\w+\\)$") str)
      (let* ((cmd (intern (format "shx-%s" (upcase (match-string 1 str))))))
        (when (fboundp cmd)
          ;; ...and such a command exists, insert triggerable text for it
          (delete-region (line-beginning-position) (line-end-position))
          (insert " ")
          (set 'shx-echo (concat "##" (match-string 1 str) "()")))))))


(defun shx-parse-output-hook (output)
  "Parse the output for triggerable text, text we want to squelch,
and other triggers like URL matching."
  (shx-parse-output-commands)                     ; shx input commands:
  (shx-check-output-for-trigger "^\033\\[[0-9]*C" ; squelch ansi artifacts
                                '(lambda () (replace-match "")))
  (shx-check-output-for-trigger "https?://[A-Za-z0-9./?=&;_-]+[^.\n\s\t\"'>)]+"
                                'shx-parse-matched-url)) ; linkify urls


(defun shx-parse-output-commands ()
  "Scan most recent output for recognized trigger text.  If some
is found, delete the text and call the corresponding function."
  ;; backup a bit...
  (goto-char comint-last-output-start)
  ;; This is necessary in case comint started reading midline:
  (let ((inhibit-field-text-motion t)) (beginning-of-line))
  ;;(insert "**") ; debug
  (while (< (point) (point-max))
    (when (re-search-forward "^##\\(\\w+\\)(\\(.*\\))" nil t)
      (let ((cmd (intern (format "shx-%s" (upcase (match-string 1))))))
        (when (and (fboundp cmd)        ; text matches a command!
                   (shx-safe-to-trigger (match-string 1))) ; safe?
          (let ((str (match-string-no-properties 2))
                (buf shx-buffer))
            (replace-match "")          ; hide the trigger text
            ;;(insert "\n")
            (funcall cmd str)
            (set-buffer buf)))))        ; (funcall may have switched our buf)
    (forward-line)))


(defun shx-check-output-for-trigger (rexp func)
  "Scan the output for the supplied REXP; on a match call func."
  (save-excursion
  (goto-char comint-last-output-start) ; go to prompt, then to the end
  (while (< (point) (point-max))
    (when (re-search-forward rexp nil t)
      (add-text-properties (match-beginning 0) (match-end 0)
                           (list 'mouse-face 'link
                                 'font-lock-face 'shx-highlights
                                 'help-echo
                                 (concat "Trigger on match: " rexp)))
      (condition-case nil (funcall func)
        (error "Sorry for the bug - can't run this trigger!!")))
    (forward-char))))


;; cribbed from emud.el
(defun shx-open-url-command (input-event)
  "Open the URL that was clicked on."
  (interactive "e")
  (let (window pos url)
    (save-excursion
      (setq window (posn-window (event-end input-event))
            pos    (posn-point  (event-end input-event)))
      (if (null (windowp window))
        (error "Unknown URL"))
      ;; Go to the buffer where the click event happened
      (set-buffer (window-buffer window))
      ;;; ^ (set-window-buffer window...??
      ;; Grab the URL, and go
      (setq url (get-text-property (1- pos) 'url)))
    (browse-url url)))


(defun shx-parse-matched-url ()
  "Assuming a match to a URL has been made in the output stream,
do some bookkeeping by adding it to the local list
`shx-urls' (accessible with C-c C-o) and make it clickable."
  (let ((url (match-string-no-properties 0)))
    (if (null (string= url (car shx-urls)))
      (setq shx-urls (cons url shx-urls)))
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-1] 'shx-open-url-command)
      (add-text-properties (match-beginning 0) (match-end 0)
                           (list 'mouse-face 'link
                                 'font-lock-face 'shx-highlights
                                 'keymap map
                                 'url url
                                 'help-echo (concat "Click or C-c C-o to open "
                                                    url))))))


;;; =====================================================================
;;;                          Utility functions


;;; TODO this might just be a macro
;;; http://www.chemie.fu-berlin.de/chemnet/use/info/elisp/elisp_13.html#SEC157
;;; macros appear to be the next thing to learn in any case...
(defun shx-delay-cmd (delay cmd &optional buf)
  "Process CMD in the current buffer after DELAY seconds.  In the
meantime the session will remain responsive.  See documentation
for `run-at-time' for further documentation on the timing
mechanism."
   (run-at-time delay nil
    ;; We want ONLY literals inside the delayed function, since these won't lose
    ;; scope or change context on us.  e.g., what `buffer-name' returns can
    ;; change as the user moves around in emacs.  So we eval an sexp which
    ;; produces an anonymous function in which the buffer's name is a literal.
    (eval (cons 'quote (list (list 'lambda 'nil
                                   (cons 'process-send-string
                                         (list (buffer-name buf) cmd))))))))


(defun shx-delay-funcall (delay func &optional args)
  "Call FUNC after DELAY. It's useful for example for making sure
the shx window is up-to-date with respect to where the cursor
should be, etc. while enabling commands that hide the shx buffer,
like `shx-EDIFF' and `shx-EHERE'."
  (run-at-time delay nil
   (eval (cons 'quote (list (list 'lambda 'nil           ; Why so ugly?  See
                                  (cons func args))))))) ; `shx-delay-cmd'...


(defun shx-general-plot (filename plot-cmd line-style)
  "Prepare a plot of the data in FILENAME using a gnuplot
specific plot-cmd (for example 'plot') and LINE-STYLE (for
example 'w lp'); insert the resulting plot into the buffer."
  (let ((imgname (make-temp-file "tmp" nil ".png")))
    (when (eq 0 (call-process shx-gnuplot-cmd nil t nil "-e"
                              (concat 
                               "set term png; "
                               "set out \"" imgname "\"; "
                               plot-cmd " \"" (expand-file-name filename) "\" "
                               line-style)))
      (shx-VIEW imgname))))


(defun shx-browse-url (&optional arg)
  ;; cribbed from rcirc.el
  "Prompt for URL to browse based on URLs in buffer.  If no URLs
have been seen yet, prompt for any URL at all."
  (interactive "P")
  (let ((completions (mapcar (lambda (x) (cons x nil)) shx-urls))
        (initial-input (or (car shx-urls) "http://"))
        (history (cdr shx-urls)))
    (browse-url
     (completing-read "Open URL: "
                      completions nil nil initial-input 'history)
     arg)))


(defun shx-send-break ()
  (interactive)
  (process-send-string nil ""))


(defun shx-space-maybe ()
  "When the prompt is just a colon, we can assume we're paging through `less',
 -- such as with a man page, for example.  In this case instead
of inserting a space in the buffer, we send it straight through
to the process so that paging happens with one key press."
  (interactive)
  (if (string= ":" (shx-get-current-prompt))
      (process-send-string nil " ")
    (insert " ")))



;;; =====================================================================
;;;                          Scrolling extras


;; These are controls for navigating a window that looks like this
"+--------------+
 | -------      | 
 | -------      | 
 | -------      | 
 |    <head>    | 
 |(show history)| 
 +--------------+ 
 |    <tail>    | 
 |(show context)|
 +--------------+"


(defun shx-scroll-begin ()
  "Create the head/tail window pair."
  (interactive)
  (goto-char (point-max))
  (save-excursion
    ;; open up a small window beneath us
    (split-window-vertically (- (window-height) shx-row-split))
    ;; remember previous comint settings
    (set (make-local-variable 'shx-default-scroll-on-output)
         comint-scroll-to-bottom-on-output)
    ;; only auto-scroll the window the user's cursor is in
    (set (make-local-variable 'comint-scroll-to-bottom-on-output) "this")))


(defun shx-scroll-find-split ()
  "The user might be on the head or the tail.  Regardless, put
cursor on the tail at the end of buffer, or return nil if the
tail is not visible and/or the matching buffer is not above it."
  (cond ((and (eq (current-buffer) (window-buffer (previous-window)))
              (eq shx-row-split (window-height)))
         ;; on the tail?  good
         t)
        ((and (eq (current-buffer) (window-buffer (next-window)))
              (eq shx-row-split (window-height (next-window)))) ; on the head?
         ;; not on the tail?  move from head to tail or return nil
         (select-window (next-window)))))


(defun shx-scroll-up (&optional home)
  "If the window is not currently split, split it (head/tail) as
long as it's big enough.  If the window is split, scroll the top
window and leave the bottom window on the prompt for entering
commands.  See `shx-scroll-down'."
  (interactive)
  (if (and (not (shx-scroll-find-split))
           (< (window-height) 40))
      ;; window too small to split?  default behavior
      (if home (goto-char (point-min))
        (scroll-down))
    ;; go to the head, or create one if it doesn't exist
    (if (shx-scroll-find-split)
        (select-window (previous-window))
      (shx-scroll-begin))
    ;; scroll appropriately
    (if home (goto-char (point-min))
      (recenter -1)
      (move-to-window-line 0)
      (recenter -1))
    ;; move down to the bottom window
    (select-window (next-window))
    ;; align text to bottom
    (goto-char (point-max))
    (recenter -1)))


(defun shx-scroll-home ()
  "shx-scroll to the top."
  (interactive)
  (shx-scroll-up 'home))


(defun shx-scroll-down ()
  "If the window is split, scroll the top window and leave the
bottom window on the prompt for entering commands.  If the top
window scrolls down to the prompt, remove the split.  See
`shx-scroll-up'."
  (interactive)
  (if (not (shx-scroll-find-split))
      (when (< (point) (point-max))     ; don't over-scroll...
        (scroll-up))
    ;; we are now on the tail, so move up to the head
    (select-window (previous-window))
    ;; scroll appropriately
    (move-to-window-line -1) (recenter 0)
    (move-to-window-line -1) (recenter -1)
    (recenter -1)
    (if (>= (point) (point-max))
        ;; did this scroll us to the bottom?  restore windows
        (shx-scroll-end)
      ;; otherwise return to the tail window
      (select-window (next-window)))
    ;; align text to bottom
    (goto-char (point-max))
    (recenter -1)))


(defun shx-scroll-end ()
  "If the window is split, remove the split.  See `shx-scroll-up'
and `shx-scroll-down'."
  (interactive)
  (when (shx-scroll-find-split)
    ;; we have found the tail and we are on it.
    ;; Go up and remove the scroll window
    (save-excursion
      (select-window (previous-window))
      (delete-window))
    ;; pop old scrolling preferences
    (set 'comint-scroll-to-bottom-on-output shx-default-scroll-on-output))
  ;; align text to bottom
  (goto-char (point-max))
  (recenter -1))

(defun shx-safe-to-trigger (arg)
  "Return t if the shx-COMMAND specified by ARG is safe to use as
a trigger (e.g., you can't have your file system nuked by running
this command in a malicious way, or it's not open to substantial
logic bombs).  It does this by looking for the string (SAFE) at
the start of the command's documentation string."
  (condition-case nil 
      (string= "(SAFE)"
               (substring (documentation
                           (intern (format "shx-%s" (upcase arg)))) 0 6))
    (error nil)))


;;; =====================================================================
;;;                     (Custom) user commands


(defun shx-HELP (arg)
  "(SAFE) Displays help on the shx user function indicated by
ARG.  If no such function exists (or none is supplied) display
general help list."
  (insert (propertize
           "** shx.el help **\n" 'font-lock-face 'shx-highlights))
  (if (fboundp (intern (format "shx-%s" (upcase arg))))
      (progn
        (insert "'" arg "':\n\t")
        (insert (documentation (intern (format "shx-%s" (upcase arg)))))
        (insert "\nUsage:\n")
        (insert "\tTo use as a command, type "
                (propertize (concat shx-prefix arg " ARG")
                            'font-lock-face 'shx-highlights)
                " at the prompt")
        (if (shx-safe-to-trigger arg)
            (insert "\n\tTo use as a trigger, output "
                    (propertize (concat "##" arg "(ARG)")
                                'font-lock-face 'shx-highlights)
                    " on a line by itself")
          (insert "\n\tNot safe to use as a trigger.") ))
    ;; It would be nice to automate this list (how?)
    (let ((nn (concat "\t" shx-prefix)))
      (insert
       "General:\n"
       nn "help\n"
       nn "help <command>             (specific help)\n"
       nn "[w]ww                      (open a link)\n"
       nn "echo <text>                (echo some text)\n"
       nn "[e]dit <filename>          (edit a file, this window)\n"
       nn "[sp]edit <filename>        (edit a file, split window)\n"
       nn "oedit <filename>           (edit a file, other window)\n"
       nn "man <command>              (show a man page)\n"
       nn "woman <command>            (show a man page)\n"
       nn "diff <file1> <file2>       (launch a diff)\n"
       nn "ediff <file1> <file2>      (launch an ediff)\n"
       nn "grep '<pattern>' <file1>   (launch a grep)\n"
       nn "delay <seconds> <command>  (send <command> after <seconds>)\n"
       nn "done                       (display 'done' message)\n"
       nn "showdone                   (like done but display buffer)\n"
       "Graphical:\n"
       nn "map <location>             (google maps)\n"
       nn "view image.png             (view any image)\n"
       nn "plot data.dat              (gnuplot lineplot)\n"
       nn "scatter data.dat           (gnuplot scatterplot)\n"
       nn "plot3d data3d.dat          (gnuplot 3D surface)\n"
       nn "barplot databars.dat       (gnuplot barplot)\n"
       nn "matrix mtx.dat             (gnuplot heatmap)\n"
       "Examples:\n"
       nn "help scatter\n"
       nn "map University of Alberta\n"
       nn "view image.png\n"
       nn "e ~\n"
       nn "plot rewardVStime.dat"))))
  


(defun shx-ECHO (arg)
  "(SAFE) Echo ARG. This function is only semiSAFE to trigger
because of potential for recursive logic bomb."
  (insert arg))


(defun shx-ECHOARGS (arg)
  "This function is for testing.  UNSAFE."
  (let ((arglist (split-string arg)))
    (insert (car arglist))
    (mapcar '(lambda (x) (insert "\n" x))
            (cdr arglist))))


(defun shx-DELAY (arg)
  "shx.el command to run a delayed shell command after some
specified number of seconds have elapsed.  Here's an example to
delay a directory listing: \":delay 3 ls\".  Definitely UNSAFE."
  (if (string-match "^[\s\t]*\\([0-9]+\\)[\s\t]+\\([^\s\t].*\\)$" arg)
      (progn
        (insert "Delaying execution of `"
                (match-string 2 arg) "' for "
                (match-string 1 arg) " seconds.")
        (shx-delay-cmd (concat (match-string 1 arg) " sec")
                       (concat (match-string 2 arg) "\n")))))


(defun shx-DIFF (arg)
  "(SAFE) shx.el command to launch a diff window in emacs."
  (insert "Invoking diff `" arg
          "' in other window; use M-n/N-p to browse results.")
  (let ((arglist (mapcar 'expand-file-name (split-string arg))))
    (diff (car arglist)
          (car (cdr arglist)))))


(defun shx-EDIFF (arg)
  "(SAFE) shx.el command to launch an ediff session."
  (insert "Invoking ediff " arg "...")
  (shx-delay-funcall "0.5 sec" 'ediff
                     (mapcar 'expand-file-name
                             (split-string arg))))


(defun shx-GREP (arg)
  "(SAFE) shx.el command to launch an emacs grep window.  Here's
an example to try: \":grep '^(defun shx-[A-Z]\+ (' shx.el\""
  (insert "Grepping " arg
          " in other window; use n/p to browse results.")
  (grep (concat "grep -nH " arg)))


(defun shx-MAN (arg)
  "(SAFE) shx.el command to launch a man window."
  (insert "Invoking 'man " arg "' in other window.")
  (man arg))


(defun shx-WOMAN (arg)
  "(SAFE) shx.el command to launch a WoMan window."
  (insert "Invoking 'woman " arg "' in other window.")
  (woman arg))


(defun shx-OEDIT (arg)
  "(SAFE) shx.el command to open the file ARG in another window.
You won't need to (and shouldn't!) escape spaces and special
characters.  That is, if you have a file called 'my\ file', you
can just open it using ':edit my file'."
  (insert "Opening " arg " in other window.")
  (display-buffer (find-file-noselect (expand-file-name arg))))


(defun shx-SPEDIT (arg)
  "(SAFE) shx.el command to open the file ARG in new split window.
You won't need to (and shouldn't!) escape spaces and special
characters.  That is, if you have a file called 'my\ file', you
can just open it using ':spedit my file'."
  (insert "Opening " arg " in split window.")
  (split-window-below)
  (other-window 1)
  (find-file (expand-file-name arg))
  (other-window -1))
(defalias 'shx-SP 'shx-SPEDIT)


(defun shx-CAT (arg)
  "(SAFE) shx.el command to cat a file into the buffer.
You won't need to (and shouldn't!) escape spaces and special
characters.  That is, if you have a file called 'my\ file', you
can just view it using ':cat my file'.  This function is only
semiSAFE to trigger because of potential for recursive logic
bomb."
  (condition-case nil 
      (insert-file-contents (expand-file-name arg))
    (error (insert arg ": No such file."))))


(defun shx-EHERE (arg)
  "(SAFE) shx.el command to open the file ARG in the current window.
You won't need to (and shouldn't!) escape spaces and special
characters.  That is, if you have a file called 'my\ file', you
can just open it using ':edit my file'."
  (insert "Editing " arg "...")
  (shx-delay-funcall "0.25 sec"
                     'find-file (list (expand-file-name arg))))
(defalias 'shx-E 'shx-EHERE)
(defalias 'shx-OPEN 'shx-EHERE)


(defun shx-WWW (arg)
  "(SAFE) shx.el command to go to URL menu."
  (insert "WWW...")
  (shx-browse-url))
(defalias 'shx-W 'shx-WWW)


(defun shx-DONE (arg)
  "(SAFE) shx.el command to message the user when the shell
buffer isn't visible."
  (insert "shx.el: ** done **")
  (message (format "** shx job finished in %S **" shx-buffer)))


(defun shx-SHOWDONE (arg)
  "(SAFE) shx.el command to message the user when the shell
buffer isn't visible."
  (insert "shx.el: ** done **")
  (message (format "** shx job finished in %S **" shx-buffer))
  (display-buffer shx-buffer)
  ;; IDEA Show window buffer using set-window-buffer...!
  )


(defun shx-MAP (arg)
  "(SAFE) shx.el command (though not a very useful one) to
produce a Google map mage from a location string ARG that Google
Maps can parse... this can be a street, city, lat/long, whatever.
e.g., \":map University of Alberta\".  Note this function
requires wget to be installed in addition to ImageMagick."
  (let ((imgname (make-temp-file "tmp" nil ".png")))
    (call-process shx-wget-cmd nil nil nil
                  "-nv" "-O" imgname
                  (concat (shx-gmap-url) arg))
    (shx-VIEW imgname))
  (insert "http://maps.google.ca/maps?q="
          (replace-regexp-in-string " " "%20" arg)))


(defun shx-VIEW (arg)
  "(SAFE) Prepare (ie scale) the image indicated by ARG using
convert; insert the resulting plot into the buffer.  Note this
function requires ImageMagick to be installed."
  (let ((imgname (make-temp-file "tmp" nil ".png")))
    (when (eq 0 (call-process shx-convert-cmd nil t nil
                              (expand-file-name arg)
                              "-resize" (concat
                                         "x" (number-to-string shx-imgsize) ">")
                              imgname)))
         (insert-image (create-image imgname))
         (insert "\n^ " imgname "\n")))


(defun shx-BARPLOT (arg)
  "(SAFE) Show a barplot of the file indicated by ARG containing, e.g.:
\"Topic 1\" 1.8
\"Topic 2\" 19.5
\"Topic 3\" 4
\"T\\374opic 4\", 20
Note this function requires gnuplot and ImageMagick to be
installed."
  (shx-general-plot arg "set boxwidth 2.5 relative; set style data histograms;
                         set style fill solid 1.0 border -1;
                         plot" "u 2:xticlabels(1) notitle"))


(defun shx-PLOT (arg)
  "(SAFE) Use gnuplot to show a line plot of the file named ARG.
Note this function requires gnuplot and ImageMagick to be
installed."
  (shx-general-plot arg "plot" "w lp lw 1 ps 2 pt 7"))


(defun shx-MATRIX (arg)
  "(SAFE) Use gnuplot to show a heatmap of the matrix contained
in file ARG such as:
1.5   2    3
4     5    6
7     8    9.5
Note this function requires gnuplot and ImageMagick to be
installed."
  (shx-general-plot arg "set view map; unset xtics; unset title; unset ytics; set colorbox;
                         plot" "u 1:(-$2):3 matrix w image"))


(defun shx-SCATTER (arg)
  "(SAFE) Use gnuplot to show a scatter plot of the file ARG.
Note this function requires gnuplot and ImageMagick to be
installed."
  (shx-general-plot arg "plot" "w p ps 2 pt 7"))


(defun shx-PLOT3D (arg)
  "(SAFE) Use gnuplot to show a surface plot of the file ARG.
Note this function requires gnuplot and ImageMagick to be
installed."
  (shx-general-plot ARG "unset tics;
                         set palette defined
                         ( 0 \"black\",2 \"blue\",3 \"#ddccbb\",4 \"#00cc00\");
                         set view 0,0,1.5,1;
                         splot" "w pm3d"))


(defun shx-UPDATE (arg)
  "(SAFE) Update the most recently embedded image or plot..."
  (insert "Not implemented."))


(defun shx-TEST (arg)
  "(SAFE)"
  (insert "Testing...")
  (shx-tests))

;;; =====================================================================
;;;                           Test cases ...


(defun shx-tests ()
  "Run through some test cases to ensure certain behaviors hold."
  (interactive)
  (goto-char (point-max))
  ;; scrolling tests
  (if (< (window-height) 40)
      (message "Scroll tests need window-height >= 40.")
    (assert (let ((currpt (point)))
              (shx-scroll-up) (shx-scroll-end) (eq currpt (point))))
    (assert (let ((currpt (point)))
              (shx-scroll-home) (shx-scroll-down) (eq currpt (point))))
    (assert (let ((currpt (point)))
              (shx-scroll-end) (eq currpt (point))))
    (assert (let ((currpt (point)))
              (shx-scroll-home) (eq currpt (point))))
    (assert (let ((currpt (point)))
              (shx-scroll-home) (shx-scroll-end) (eq currpt (point))))
    (assert (let ((currpt (point)))
              (shx-scroll-end) (shx-scroll-up)
              (shx-scroll-down) (shx-scroll-down) (eq currpt (point))))
    (assert (null (shx-scroll-find-split)))
    (message "Scrolling tests passed!"))
  (assert (numberp shx-imgsize))
  (assert (> shx-imgsize 0))
  (message "Variable tests passed!")
  t)


;;; =====================================================================
;;;                             Loading ...


(defun shx ()
  "Make a new shx buffer.  Provides the following keybindings:
\n\\{shx-mode-map}"
  (interactive)
  (shell (generate-new-buffer-name "*shx*"))
  (shx-activate))


(defun shx-activate ()
  (interactive)
  (message "shx activated")
  (buffer-disable-undo)
  (use-local-map (shx-get-mode-map (current-local-map)))
  (set (make-local-variable 'shx-echo) nil)
  (set (make-local-variable 'shx-urls) nil)
  (set (make-local-variable 'shx-buffer) (current-buffer))
  (setq comint-input-ring-size 500)
  (add-hook 'comint-input-filter-functions 'shx-show-echo-hook
            nil t)
  (add-hook 'comint-output-filter-functions
            'shx-parse-output-hook nil t))


;; Tell less to suppress warnings about how dumb our terminal is, and to
;; use a consistently simple prompt (just a colon).
(setenv "LESS" "--dumb --prompt=s")

(add-hook 'comint-mode-hook 'shx-activate)
(add-hook 'py-shell-hook 'shx-activate)

(provide 'shx)
;;; shx.el ends here

