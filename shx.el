;;; shx.el --- shell-extras, extras for the (comint-mode) shell
;;
;; Copyright (C) 2012,2013,2014 Chris Rayner
;;
;; Author: rayner AT cs DOT ualberta DOT ca
;; Created: (Mon May 23 2011)
;; Updated: (Tue Jan 14 2014) 
;; Version: alpha
;; Keywords: comint-mode, shell-mode, shell-extras
;; URL: http://www.cs.ualberta.ca/~rayner/
;; Git: https://github.com/riscy/shx-for-emacs

;;; Commentary:
;;
;; `shx' or "shell-extras" extends comint-mode:
;;
;; - It parses simple markup in the output stream
;;   - Plots/graphics can be automatically embedded in the shell
;;   - Links become clickable and keyboard accessible
;;   - An Emacs-wide alert can tell the user a task is done
;; - It adds several command-line functions which plug into Emacs
;;   - Examine a plot of some data file without changing context
;;   - Display a (clickable) Google Maps image of a given location
;;   - Adding new functions is easy (for an example see below)
;; - It automatically splits the screen when you page up and down
;;   - This lets you keep context when referring to earlier output
;;
;; This version tested with Emacs 24.0.94 on Mac OS X.
;;
;; shx comes without any warranty.
;;
;;
;; Installation
;; ============
;;
;; 1. Move shx.el to a directory in your `load-path'.  Or, add
;;    shx.el's directory to your `load-path' by adding a line like
;;    this to your .emacs:
;; (add-to-list 'load-path "~/path/to/elisp/")
;;
;; 2. Next add this line to your .emacs:
;; (require 'shx)
;;
;; If you want shx to run in ANY comint-mode buffer, add this too:
;; (add-hook 'comint-mode-hook 'shx-activate)
;;
;; That's it!
;;
;;
;; Quick-Start
;; ===========
;;
;; 1. Finish the installation (as above).
;; 2. Type M-x shx (enter) to begin a shell session using shx
;; 3. Type :man ls
;; 4. Type :delay 2 echo -e "\n##done(TEST)"
;; 5. Type :help
;; 6. Type :test (hopefully you see nothing but a success message)
;; 7. Try to page up, enter a command, then page back down
;;
;; Detailed help can be found in the next few sections.
;;
;;
;; Requirements
;; ============
;;
;; The graphical functions (like plotting) currently use:
;; - convert (i.e., ImageMagick, used to scale images to size)
;; - gnuplot (for all plotting functions)
;;
;; You can edit shx.el (near the top) to make `shx-convert-cmd' and
;; `shx-gnuplot-cmd' point to these binaries.
;;
;;
;; shx Input Commands
;; ==================
;;
;; Everything you need to know about shx's input commands can be found
;; in the help.  Just type :help on an empty line and press enter.
;;
;; Many shx commands are for displaying graphics such as plots in a
;; shell buffer.  These require ImageMagick and gnuplot to be
;; installed.  Others invoke built-in Emacs functionality, like :man,
;; :edit, :grep, :delay.  Nothing extra is needed in this case.
;;
;; You can change the prefix you type at the prompt before shx
;; commands from ":" to "# " by putting this line in your .emacs,
;; (setq shx-prefix "# ")
;; in which case you would type "# help" to access the help.  Or you
;; can set the prefix to nothing all:
;; (setq shx-prefix "")
;; in which case you would type "help" to access the help.
;;
;;
;; shx Input Commands -- roll your own
;; ===================================
;;
;; Skip this section unless you're interested in how shx works and
;; maybe in writing your own input commands.
;;
;; shx's support for input commands written in elisp gives it a lot of
;; the same advantages as `eshell'.  Users can write new commands by
;; defining a single-argument elisp function named shx-COMMAND, where
;; COMMAND (which must be capitalized) is what the user would type to
;; invoke it.  For example if you put this in your .emacs:
;;
;; (defun shx-BREAK (arg) (insert "Break!") (shx-send-break))
;;
;; ... a user can type :break to send a break straight through.  See
;; `shx-DIFF', `shx-GREP' for examples.
;;
;; If you write a new command that you think might be useful to
;; others, send it along to me and hopefully I can include it.
;;
;;
;; shx Command Triggers
;; ====================
;;
;; Triggers can be used to enhance command-line applications.  They
;; are invoked by a simple markup language: just have the application
;; output something like the following on a line by itself:
;; ##COMMAND(ARGUMENT)
;; where COMMAND is one of shx's input commands.
;; 
;; For example, if shx sees the following line:
;; ##view(mountains.png)
;; then mountains.png will be displayed in the shell, scaled to fit.
;;
;; You can control how much vertical space any image occupies by
;; customizing the variable `shx-imgsize', or by executing:
;; (setq shx-imgsize 300)
;;
;;
;; shx Scrolling
;; =============
;;
;; By default, shx splits the window on paging up and down.  When you
;; page up, the frame is split in two with a larger "scrolling frame"
;; on top and a smaller "input frame" preserved on the bottom.  This
;; lets you enter text at the prompt (in the input frame) and monitor
;; new input while consulting previous output (in the scrolling frame)
;; uninterrupted.
;;
;; You can change the size of the input frame to something else:
;; (setq shx-split-rows 15)
;;
;; Or disable this feature entirely:
;; (setq shx-split-rows 0)
;;
;; NOTE: typing :test will fail if you disable this feature.
;;
;;
;; shx Keys
;; ========
;;
;; - C-c C-c sends a break character (C-c).
;;
;; - C-c C-z sends a stop character (C-z).
;;
;; - C-c C-k sends a SIGKIL to the subjob (what C-c C-c did before).
;;
;; - Recognized URLs are turned into mouse/keyboard accessible links
;;   (C-c b) and a history of previous links is maintained.
;;
;; - When the prompt is a ":" (such as when reading through a man
;;   page), leading spaces and 'q's are sent straight to the process
;;   rather than being inserted into the buffer.
;;
;;
;; Priorities
;; ==========
;;
;; - REPEAT command?
;; - Command to save an image (matrix, etc) rather than just display it.
;; - When window is slightly smaller, don't overscroll with pgup
;;
;;
;; Recent
;; ======
;;
;; - Massive performance improvements in the triggering code
;; - Better equipped for laggy buffers and multiple triggers on one line
;; - Optional custom triggers in a list (export the URL one there first)

;;; Code:


(require 'comint)


;;; =====================================================================
;;;              Global variables and customization options


;; Programs used (try absolute paths if it's not working which can
;; be determined e.g. using 'which convert' or 'which gnuplot')
(defvar shx-convert-cmd "convert")
(defvar shx-gnuplot-cmd "gnuplot")


;; List of triggers and commands (for example, URL matching)
(defvar shx-triggers
  (list
   '("https?://[A-Za-z0-9,./?=&;_-]+[^.\n\s\t\"'>)]+" . shx-parse-matched-url) ))


;; Some other variables
(defvar shx-prefix ":")
(defvar shx-imgsize 200)
(defvar shx-split-rows 15)


(defun shx-gmap-url ()
  (concat "http://maps.google.com/maps/api/staticmap"
          "?maptype=roadmap&zoom=13&sensor=false&size="
          (number-to-string shx-imgsize) "x"
          (number-to-string shx-imgsize) "&markers="))


(defun shx-get-mode-map (&optional parent)
  "Keymap used for `shx-mode'."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map parent)
    ;; Intercept keys that are used when using 'less':
    (define-key map (kbd "SPC") 'shx-context-sensitive-insert)
    (define-key map (kbd "q") 'shx-context-sensitive-insert)
    (define-key map (kbd "b") 'shx-context-sensitive-insert)
    (define-key map (kbd "/") 'shx-context-sensitive-insert)
    (define-key map (kbd "?") 'shx-context-sensitive-insert)
    (define-key map (kbd "p") 'shx-context-sensitive-insert)
    (define-key map (kbd "n") 'shx-context-sensitive-insert)
    (define-key map (kbd "N") 'shx-context-sensitive-insert)
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
    (define-key map (kbd "C-c C-z") 'shx-send-stop)
    (define-key map (kbd "C-c C-k") 'comint-kill-subjob)
    ;; Send a command
    (define-key map (kbd "<return>") 'shx-send-input)
    map))


(defvar shx-mode-map (shx-get-mode-map))


(defface shx-highlights
  '((((class color)) (:foreground "#00AAEE")))
  "Face to highlight user input that went through shx."
  :group 'shx)


;;; =====================================================================
;;;                    Hooks and input commands


(defun shx-get-current-input ()
  "Return what's written after the prompt (i.e., the string
between the `process-mark' and the `line-end-position')."
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


(defun shx-show-trigger-hook (input)
  "When the user hits enter (sending the input), check the local
variable `shx-user-trigger' for content to insert into the output
stream, such as some triggerable text (see
`shx-parse-output-hook').  This function should be in
`comint-input-filter-functions'."
  (when (not (eq shx-user-trigger nil))
    (save-excursion
      (goto-char (point-max))
      (forward-line 0)
      (backward-char)
      (insert
       ;; Anything the user inputs is deemed shx-safe:
       (propertize (concat "\n" shx-user-trigger) 'shx-safe t))
    (set 'shx-user-trigger nil))))


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
                            (concat "Parsed by shx.el (shx-user-trigger " parsed ")")))))))


(defun shx-parse-input-commands (str)
  "Scan the text STR written at the prompt.  If STR starts with
shx-prefix and specifies a valid user command (e.g., :view would
specify `shx-VIEW'), swap what's written after the prompt with a
placeholder.  We will save a triggerable string to
`shx-user-trigger' (e.g., shx-user-trigger ==
'##view(image.png)') which will later be inserted in the output
stream by `shx-show-trigger-hook'."
  (interactive)
  (if (string-match (concat "^" shx-prefix "\\(\\w+\\)[\s\t]+\\(.*[^\s\t]\\)[\s\t]*") str)
      ;; If the input matches <shx-prefix><cmd> <arg>:
      (let* ((cmd (intern (format "shx-%s" (upcase (match-string 1 str))))))
        ;; If such a command exists
        (when (fboundp cmd)
          ;; clear the text after the prompt; this is done so
          ;; comint-mode will just send a blank line to the process
          (delete-region (line-beginning-position) (line-end-position))
          ;; enqueue the trigger
          (set 'shx-user-trigger
               (concat "##" (match-string 1 str) "(" (match-string 2 str) ")"))))
    ;; Otherwise if the input matches <shx-prefix><cmd>:
    (when (string-match (concat "^" shx-prefix "\\(\\w+\\)$") str)
      (let* ((cmd (intern (format "shx-%s" (upcase (match-string 1 str))))))
        (when (fboundp cmd)
          ;; ...and such a command exists, insert triggerable text for it
          (delete-region (line-beginning-position) (line-end-position))
          (insert " ")
          (set 'shx-user-trigger (concat "##" (match-string 1 str) "()")))))))


(defun shx-parse-output-hook (output)
  "Parse the output for triggerable text, text we want to squelch,
and other triggers like URL matching."
  ;; Any output commands
  (shx-parse-output-commands)
  ;; Any triggers seen
  (dolist (ii shx-triggers nil)
    (shx-check-output-for-trigger (car ii) (cdr ii))))


(defun shx-parse-output-commands ()
  "Scan most recent output for recognized trigger text.  If some
is found, delete the text and call the corresponding function."
  (save-excursion
    (goto-char comint-last-output-start)
    (forward-line 0)
    (while (and
            (< (line-number-at-pos (point)) (line-number-at-pos (point-max)))
            (re-search-forward "^##\\(\\w+\\)(\\(.*\\))" nil t))
      (let ((cmd (intern (format "shx-%s" (upcase (match-string 1))))))
        (when (and (fboundp cmd)        ; text matches a command
                   (shx-safe-to-trigger (match-string 1))) ; safe?
          (let ((str (match-string-no-properties 2))
                (buf shx-buffer))
            (replace-match "")          ; hide the trigger text
            (funcall cmd str)
            (set-buffer buf))))))) ; (funcall may have switched our buf)


(defun shx-check-output-for-trigger (rexp func)
  "Scan the output for the supplied REXP; on a match call func."
  (save-excursion
    (goto-char comint-last-output-start)
    (forward-line 0)
    (while (and (< (line-number-at-pos (point)) (line-number-at-pos (point-max)))
                (re-search-forward rexp nil t))
        (condition-case nil (funcall func)
          (error "shx error :: error on trigger funcall")))))


(defun shx-parse-matched-url ()
  "Assuming a match to a URL has been made in the output stream,
do some bookkeeping by adding it to the local list
`shx-urls' (accessible with C-c b) and make it clickable."
  (let ((url (match-string-no-properties 0)))
    (if (null (string= url (car shx-urls)))
        (setq shx-urls (cons url shx-urls)))
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-1] 'shx-open-url)
      (add-text-properties (match-beginning 0) (match-end 0)
                           (list 'mouse-face 'link
                                 'font-lock-face 'shx-highlights
                                 'keymap map 'url url
                                 'help-echo url)))))


(defun shx-highlight-match ()
  "Assuming a match to a URL has been made in the output stream,
do some bookkeeping by adding it to the local list
`shx-urls' (accessible with C-c b) and make it clickable."
  (add-text-properties (match-beginning 0) (match-end 0)
                           (list 'mouse-face 'link
                                 'font-lock-face 'shx-highlights
                                 'help-echo "Match")))


(defun shx-open-url (event)
  "Open the URL that was clicked on."
  (interactive "e")
  (browse-url (get-text-property
               (posn-point (event-start event)) 'url)))



;;; =====================================================================
;;;                          Utility functions


(defun shx-safe-to-trigger (arg)
  "Return t if the shx-COMMAND specified by ARG is safe to use as
a trigger.  If the text has the property 'shx-safe (i.e., the
user requested this command directly) or the corresponding
function has the string (SAFE) at the start of its documentation,
then it's safe to trigger."
  (or
   (get-text-property 0 'shx-safe arg)
   (condition-case nil 
       (string= "(SAFE)"
                (substring (documentation
                            (intern (format "shx-%s" (upcase arg)))) 0 6))
     (error nil))))


(defun shx-delay-cmd (delay cmd &optional buf)
  "Process CMD in the current buffer after DELAY seconds.  In the
meantime the session will remain responsive.  See documentation
for `run-at-time' for further documentation on the timing
mechanism."
   (run-at-time delay nil
    ;; We want ONLY literals inside the delayed function, since these
    ;; won't lose scope or change context on us (e.g., what
    ;; (buffer-name) evals to can change as the user moves around).
    ;; Thus we eval an sexp which produces an anonymous function in
    ;; which the buffer's name is a literal
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
  (let ((img-name (make-temp-file "tmp" nil ".png")))
    (when (eq 0 (call-process shx-gnuplot-cmd nil t nil "-e"
                              (concat 
                               "set term png; "
                               "set out \"" img-name "\"; "
                               plot-cmd " \"" (expand-file-name filename) "\" "
                               line-style)))
      (shx-VIEW img-name))))


(defun shx-browse-url ()
  "Prompt for URL to browse based on URLs in buffer.  If no URLs
have been seen yet (or there's no preset), prompt for any URL at all."
  (interactive)
  (let ((su shx-urls))                ; delocalized copy of `shx-urls'
    (browse-url
     (completing-read "URL: " su nil nil (car su) '(su . 1)))))


(defun shx-send-break ()
  (interactive)
  (message "shx :: sending BREAK.  KILL with C-c C-k.")
  (process-send-string nil ""))


(defun shx-send-stop ()
  (interactive)
  (message "shx :: sending STOP.");
  (process-send-string nil ""))


(defun shx-context-sensitive-insert ()
  "When the prompt is just a colon (e.g., we're paging through
`less') then instead of inserting the pressed key into the
buffer, send it directly to the process."
  (interactive)
  (if (and (string= (shx-get-current-prompt) ":")
           (string= (shx-get-current-input) ""))
      (process-send-string nil (this-command-keys))
     (insert (this-command-keys))))


;;; =====================================================================
;;;                          Scrolling extras
;; These are controls for creating a history/context split on the
;; window so we can enter text while reading old output:

;; +--------------+
;; | -------      |
;; | -------      |
;; | -------      |
;; |    <head>    |
;; |(show history)|
;; +--------------+
;; |    <tail>    |
;; |(show context)|
;; +--------------+


(defun shx-scroll-begin ()
  "Create the head/tail window pair."
  (interactive)
  (goto-char (point-max))
  (save-excursion
    ;; open up a small window beneath us
    (split-window-vertically (- (window-height) shx-split-rows))
    ;; remember previous comint settings
    (set (make-local-variable 'shx-default-scroll-on-output)
         comint-scroll-to-bottom-on-output)
    ;; only auto-scroll the window the user's cursor is in
    (set (make-local-variable 'comint-scroll-to-bottom-on-output)
         "this")))


(defun shx-scroll-find-split ()
  "Whether the cursor is on the head or tail frame, put it on the
tail at the end of buffer, or return nil if the tail is not
visible and/or the matching buffer is not above it."
  (cond ((and (eq (current-buffer) (window-buffer (previous-window)))
              (eq shx-split-rows (window-height)))
         ;; on the tail?  good
         t)
        ((and (eq (current-buffer) (window-buffer (next-window)))
              (eq shx-split-rows (window-height (next-window)))) ; on the head?
         ;; not on the tail?  move from head to tail or return nil
         (select-window (next-window)))))


(defun shx-scroll-up (&optional home)
  "If the window is not currently split, split it (head/tail) as
long as it's big enough.  If the window is split, scroll the top
window and leave the bottom window on the prompt for entering
commands.  See `shx-scroll-down'."
  (interactive)
  (if (or (= shx-split-rows 0)
          (and (not (shx-scroll-find-split))
               (< (window-height) 40)))
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



;;; =====================================================================
;;;                          USER COMMANDS


(defun shx-HELP (arg)
  "(SAFE) Displays help on the shx user function indicated by
ARG.  If no such function exists (or none is supplied) display
general help list."
  (insert (propertize "** shx.el help **" 'font-lock-face 'shx-highlights) "\n")
  (if (fboundp (intern (format "shx-%s" (upcase arg))))
      (progn
        (insert "`" arg "':\n")
        (insert (documentation (intern (format "shx-%s" (upcase arg)))))
        (insert "\nUsage:\n")
        (insert "To use as a command, type "
                (propertize (concat shx-prefix arg " ARG")
                            'font-lock-face 'shx-highlights)
                " at the prompt")
        (if (shx-safe-to-trigger arg)
            (insert "\nTo use as a trigger, output "
                    (propertize (concat "##" arg "(ARG)")
                                'font-lock-face 'shx-highlights)
                    " on a line by itself")
          (insert "\nNOT safe to use as a trigger.") ))
    ;; It would be nice to automate this list (how?)
    (let ((nn (concat "   " shx-prefix)))
      (insert
       nn "help <command>          specific help\n"
       nn "[e]dit <filename>       edit a file, this window\n"
       nn "[sp]edit <filename>     edit a file, split window\n"
       nn "[o]edit <filename>      edit a file, other window\n"
       nn "man <command>           show a man page, other window\n"
       nn "diff <file1> <file2>    launch a diff\n"
       nn "ediff <file1> <file2>   launch an ediff\n"
       nn "grep '<pattern>' <file> launch a grep\n"
       nn "delay <sec> <command>   send <command> after <seconds>\n"
       nn "done                    display 'done' message\n"
       nn "showdone                like done but display buffer\n"
       nn "echo <text>             echo text in the buffer\n"
       nn "mail <address>          start an email to <address>\n"
       nn "hello                   start an email to the author\n"
       nn "scatter data.dat        gnuplot scatterplot\n"
       nn "plot data.dat           gnuplot lineplot\n"
       nn "plot3d data3d.dat       gnuplot 3D surface\n"
       nn "barplot databars.dat    gnuplot barplot\n"
       nn "matrix mtx.dat          gnuplot heatmap\n"
       nn "map <location>          google maps\n"
       nn "view image.png          view any image\n"))
    (insert (propertize "Type :help <topic> for more detail (e.g. 'help plot')"
                        'font-lock-face 'shx-highlights))))


(defun shx-ECHO (arg)
  "Echo ARG.  UNSAFE (can recurse)."
  (insert arg))


(defun shx-ECHOARGS (arg)
  "This function is for testing.  UNSAFE (can recurse)."
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
        (insert "shx :: delaying execution of `"
                (match-string 2 arg) "' "
                (match-string 1 arg) " seconds.")
        (shx-delay-cmd (concat (match-string 1 arg) " sec")
                       (concat (match-string 2 arg) "\n")))))


(defun shx-DIFF (arg)
  "(SAFE) shx.el command to launch an Emacs diff window."
  (insert "shx :: invoking diff `" arg
          "' in other window; use M-n/N-p to browse results.")
  (let ((arglist (mapcar 'expand-file-name (split-string arg))))
    (diff (car arglist)
          (car (cdr arglist)))))


(defun shx-EDIFF (arg)
  "(SAFE) shx.el command to launch an Emacs ediff session."
  (insert "shx :: invoking ediff " arg "...")
  (shx-delay-funcall "0.5 sec" 'ediff
                     (mapcar 'expand-file-name
                             (split-string arg))))


(defun shx-GREP (arg)
  "(SAFE) shx.el command to launch an Emacs grep window.  Here's
an example to try:
:grep '^(defun shx-[A-Z]' shx.el"
  (insert "shx :: grepping " arg
          " in other window; use n/p to browse results.")
  (grep (concat "grep -nH " arg)))


(defun shx-MAN (arg)
  "(SAFE) shx.el command to launch a man window."
  (insert "shx :: invoking 'man " arg "' in other window.")
  (man arg))


(defun shx-OEDIT (arg)
  "(SAFE) shx.el command to open the file ARG in another window.
You won't need to (and shouldn't!) escape spaces and special
characters.  That is, if you have a file called 'my\ file', you
can just open it using ':edit my file'."
  (insert "shx :: opening " arg " in other window.")
  (display-buffer (find-file-noselect (expand-file-name arg))))

(defalias 'shx-O 'shx-OEDIT)


(defun shx-SPEDIT (arg)
  "(SAFE) shx.el command to open the file ARG in new split window.
You won't need to (and shouldn't!) escape spaces and special
characters.  That is, if you have a file called 'my\ file', you
can just open it using ':spedit my file'."
  (insert "shx :: opening " arg " in split window.")
  (split-window-below)
  (other-window 1)
  (find-file (expand-file-name arg))
  (other-window -1))

(defalias 'shx-SP 'shx-SPEDIT)


(defun shx-CAT (arg)
  "shx.el command to cat a file into the buffer.
You won't need to (and shouldn't!) escape spaces and special
characters.  That is, if you have a file called 'my\ file', you
can just view it using ':cat my file'."
  (condition-case nil 
      (insert-file-contents (expand-file-name arg))
    (error (insert arg ": No such file."))))


(defun shx-EDIT (arg)
  "(SAFE) shx.el command to open the file ARG in the current window.
You won't need to (and shouldn't!) escape spaces and special
characters.  That is, if you have a file called 'my\ file', you
can just open it using ':edit my file'."
  (insert "shx :: editing " arg "...")
  (shx-delay-funcall "0.5 sec"
                     'find-file (list (expand-file-name arg))))

(defalias 'shx-E 'shx-EDIT)


(defun shx-MAIL (arg)
  "(SAFE) shx.el command to start an email to ARG."
  (insert "shx.el :: emailing " arg)
  (shx-delay-funcall "0.5 sec" 'browse-url (list (concat "mailto:" arg))))


(defun shx-HELLO (arg)
  "(SAFE) shx.el command to start an email to the author."
  (insert "shx.el :: emailing the author at rayner@cs.ualberta.ca")
  (shx-delay-funcall "0.5 sec" 'browse-url
                     (list "mailto:rayner@cs.ualberta.ca")))


(defun shx-DONE (arg)
  "(SAFE) shx.el command to message the user even when the shell
buffer isn't visible (e.g., so a process can tell emacs it has finished).  ARG is an
accompanying string."
  (insert "shx.el :: ** done ** " arg)
  (message (format "** Done in %S ** %s" shx-buffer arg)))


(defun shx-SHOWDONE (arg)
  "(SAFE) shx.el command to display the shell buffer in the other
window to the user (e.g., so a process can tell emacs it has
finished).  ARG is an accompanying string."
  (insert "shx.el :: ** done ** " arg)
  (message (format "** Done in %S ** %s" shx-buffer arg))
  (display-buffer shx-buffer))


(defun shx-MAP (arg)
  "(SAFE) shx.el command which takes a location string ARG and
creates a clickable image from Google maps.  ARG can be a city,
place, lat/long, etc.  e.g., \":map University of Alberta\".
Note this is a little slow the first time you call it!  Requires
ImageMagick to be installed."
  (let ((img-name (make-temp-file "tmp" nil ".png"))
        (oldpoint (point))
        (url (concat "http://maps.google.ca/maps?q="
                     (replace-regexp-in-string " " "%20" arg)))
        (map (make-sparse-keymap)))
    (define-key map [mouse-1] 'shx-open-url)
    (save-excursion 
      (url-copy-file
       (concat (shx-gmap-url)
               (replace-regexp-in-string " " "%20" arg)) img-name t))
    (shx-VIEW img-name)
    (add-text-properties oldpoint (point)
                         (list 'mouse-face 'link
                               'keymap map
                               'url url
                               'help-echo url))))


(defun shx-VIEW (arg)
  "(SAFE) Prepare (ie scale) the image indicated by ARG using
convert; insert the resulting plot into the buffer.  Requires
ImageMagick to be installed."
  (let ((img-name (make-temp-file "tmp" nil ".png")))
    (when (eq 0 (call-process shx-convert-cmd nil t nil
                              (expand-file-name arg)
                              "-resize" (concat
                                         "x" (number-to-string shx-imgsize) ">")
                              img-name)))
    (let ((oldpoint (point)))
         (insert-image (create-image img-name))
         (add-text-properties oldpoint (point)
                              (list 'help-echo arg))))
  (insert "\n" arg))


(defun shx-BARPLOT (arg)
  "(SAFE) Show a barplot of file ARG, e.g.:
\"Topic 1\" YHEIGHT1
\"Topic 2\" YHEIGHT2
\"Topic 3\" YHEIGHT3
\"Topic 4\" YHEIGHT4
Requires gnuplot and ImageMagick to be installed."
  (shx-general-plot arg "set boxwidth 1.5 relative;
                         set style data histograms;
                         set xtic rotate by -40 scale 0 font \",10\";
                         set yrange [0:];
                         set style fill solid 1.0 border -1;
                         plot" "u 2:xticlabels(1) notitle"))


(defun shx-BARPLOT2 (arg)
  "(SAFE) Show a variable-width barplot of file ARG, e.g.:
\"Topic 1\" XCENTER1 YHEIGHT1
\"Topic 2\" XCENTER2 YHEIGHT2
\"Topic 3\" XCENTER3 YHEIGHT3
\"Topic 4\" XCENTER4 YHEIGHT4
Requires gnuplot and ImageMagick to be installed."
  (shx-general-plot arg "set style fill solid 1.0 border -1;
                         plot" "u 2:3:xticlabels(1) with boxes notitle"))


(defun shx-BARPLOT3 (arg)
  "(SAFE) Show a variable-width barplot of file ARG, e.g.:
\"Topic 1\" XCENTER1 BUCKETNUM YHEIGHT1 BUCKETWIDTH
\"Topic 2\" XCENTER2 BUCKETNUM YHEIGHT2 BUCKETWIDTH
\"Topic 3\" XCENTER3 BUCKETNUM YHEIGHT3 BUCKETWIDTH
\"Topic 4\" XCENTER4 BUCKETNUM YHEIGHT4 BUCKETWIDTH
Requires gnuplot and ImageMagick to be installed."
  (shx-general-plot arg "set style fill solid 1.0 border -1;
                         set grid;
                         set yrange [0.9:1];
                         plot" "u 3:4:5:xticlabels(1) with boxes notitle lc 3"))


(defun shx-PLOT (arg)
  "(SAFE) Use gnuplot to show a line plot of the file named ARG.
Requires gnuplot and ImageMagick to be installed."
  (shx-general-plot arg "plot" "w lp lw 1 ps 2 pt 7 notitle"))


(defun shx-SCATTER (arg)
  "(SAFE) Use gnuplot to show a scatter plot of the file ARG.
Requires gnuplot and ImageMagick to be installed."
  (shx-general-plot arg "plot" "w p ps 2 pt 7 notitle"))


(defun shx-MATRIX (arg)
  "(SAFE) Use gnuplot to show a heatmap of the matrix contained
in file ARG such as:
1.5   2    3
4     5    6
7     8    9.5
Requires gnuplot and ImageMagick to be installed."
  (shx-general-plot arg "set view map; unset xtics; unset title; unset ytics; set colorbox;
                         set palette defined ( 0 \"black\", 1 \"blue\", 2 \"red\", 3 \"orange\" ) ;
                         plot" "u 1:(-$2):3 matrix w image notitle"))
;set palette defined ( 20 \"#101010\", 30 \"#ff0000\", 40 \"#00ff00\", 50 \"#e0e0e0\" ) ;
;set palette model HSV defined ( 0 0 1 1, 1 1 1 1 );
;set palette defined ( 0 \"#ffffff\", 2 \"#303030\");


(defun shx-PLOT3D (arg)
  "(SAFE) Use gnuplot to show a surface plot of the file ARG.
Read about gnuplot's expectations of the data:
http://t16web.lanl.gov/Kawano/gnuplot/plotpm3d-e.html
This requires gnuplot and ImageMagick to be installed."
  (shx-general-plot arg "unset tics;
                         set view 4, 20, 1.4, 1;
                         splot" "w pm3d notitle"))


(defun shx-TEST (arg)
  "(SAFE) Run tests."
  (insert "shx :: running tests - I'll let you know if something breaks.")
  (shx-tests))


;;; =====================================================================
;;;                           Test cases ...


(defun shx-tests ()
  "Run through some test cases to ensure certain behaviors hold."
  (interactive)
  (goto-char (point-max))
  ;; scrolling tests
  (if (or (< (window-height) 40)
          (= shx-split-rows 0))
      (error "Make sure the window height >= 40 and `shx-split-rows' != 0"))
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
  (assert (numberp shx-imgsize))
  (assert (> shx-imgsize 0))
  (message "Success!"))


;;; =====================================================================
;;;                          Loading shx ...


(defun shx-activate ()
  (interactive)
  (message "shx activated")
  (buffer-disable-undo)
  (use-local-map (shx-get-mode-map (current-local-map)))
  (set (make-local-variable 'shx-user-trigger) nil)
  (set (make-local-variable 'shx-urls)
       (list "https://github.com/riscy/shx-for-emacs/blob/master/README.md"))
  (set (make-local-variable 'shx-buffer) (current-buffer))
  (setq comint-input-ring-size 500)
  (add-hook 'comint-input-filter-functions 'shx-show-trigger-hook
            nil t)
  (add-hook 'comint-output-filter-functions
            'shx-parse-output-hook nil t))

;; Tell less to suppress warnings about how dumb our terminal is, and to
;; use a consistently simple prompt (just a colon).
(setenv "LESS" "--dumb --prompt=s")


(defun shx ()
  "Make a new shx buffer.  Provides the following keybindings:
\n\\{shx-mode-map}"
  (interactive)
  (shell (generate-new-buffer-name "*shx*"))
  (shx-activate))


;; Or just make these modes activate it automatically:
(add-hook 'comint-mode-hook 'shx-activate)
(add-hook 'py-shell-hook 'shx-activate)

(provide 'shx)
;;; shx.el ends
