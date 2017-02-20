;;; shx.el -- convenience functions for the (comint-mode) shell

;; Authors: Chris Rayner (dchrisrayner @ gmail)
;; Created: May 23 2011
;; Keywords: comint-mode, shell-mode
;; Git: https://github.com/riscy/shx-for-emacs

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; shx or "shell-extras" extends comint-mode:
;; - It parses simple markup in the output stream
;;   - Plots/graphics can be automatically embedded in the shell
;;   - Links become clickable and keyboard accessible (either through
;;     <return> or C-c C-b)
;; - It adds several command-line functions which plug into Emacs
;;   - :e to edit a file, or :f to fuzzy-find files.
;;   - :plot <file> to display an image plotting some data
;;   - ...and adding new functions is easy.
;;
;; This version tested with Emacs 25.1.1
;;
;; See README.org for more details.

;;; Installation:

;; 1. Move shx.el to a directory in your load-path or add
;;    this to your .emacs:
;;    (add-to-list 'load-path "~/path/to/this-file/")
;; 2. Next add this line to your .emacs:
;;    (require 'shx)
;;
;; By default, shx runs automatically in comint-mode buffers.
;;
;; Use M-x customize-group RET shx RET to see customization options.

(require 'comint)
(require 'shell)
(require 'evil)

;;; Code:


;;; customization options and other variables

(defgroup shx nil
  "Extras for the (comint-mode) shell."
  :prefix "shx-"
  :group 'comint
  :link '(url-link
          :tag "Github"
          "https://github.com/riscy/shx-for-emacs"))

(defcustom shx-path-to-convert "convert"
  "Path to ImageMagick's convert binary.")

(defcustom shx-path-to-gnuplot "gnuplot"
  "Path to gnuplot binary.")

(defcustom shx-img-height 300
  "Height to display inline images at.")

(defcustom shx-leader ":"
  "Prefix for calling user commands.")

(defcustom shx-auto-run nil
  "Whether to automatically run shx in all comint sessions.")

(defcustom shx-triggers
  '(("https?://[A-Za-z0-9,./?=&;_-]+[^.\n\s\"'>)]+" . shx--parse-url))
  "Triggers of the form: (regexp . function).")

(defvar shx-shell-mode-font-locks
  '(("#.*\\'"                0 'font-lock-comment-face)
    ("\"[^\"]*\"?"           0 'font-lock-string-face)
    ("'[^']*'?"              0 'font-lock-string-face)
    (">>"                    0 'font-lock-keyword-face)
    ("<<"                    0 'font-lock-keyword-face)
    ("&&"                    0 'font-lock-keyword-face)
    ("|"                     0 'font-lock-keyword-face)
    ("\\(:[^ \t]+\\).*\\'"   1 'font-lock-constant-face)
    ("\\(\\<git\\>\\) .*\\'" 1 'font-lock-constant-face)
    ("\\(\\<rm\\>\\) .*\\'"  1 'font-lock-warning-face))
  "Some additional syntax highlighting for shell-mode.")

(defvar shx-cmd-prefix "shx-cmd/"
  "Prefix for user-command functions.")

(defvar shx-cmd-syntax "\\(\\w+\\)[\s\t]*\\(.*[^\s\t]?\\)"
  "Regex for recognizing shx commands in input or markup.")

(defvar shx-markup-syntax (concat "^<" shx-cmd-syntax ">$")
  "Regex for recognizing shx commands in markup.")

(defvar-local shx-buffer nil
  "Local reference to the shx buffer.")

(defvar-local shx-urls nil
  "Local record of URLs seen.")

(defvar shx-click (let ((keymap (make-sparse-keymap)))
                    (define-key keymap [mouse-1] 'ffap-at-mouse)
                    keymap)
  "Keymap for capturing mouse clicks on filenames.")


;;; input

(defun shx-get-keymap (&optional parent)
  "Keymap used for shx; inherits PARENT."
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap (or parent (current-local-map)))
    ;; send these characters straight through in some circumstances
    (define-key keymap " " #'shx-send-or-insert)
    (define-key keymap "q" #'shx-send-or-insert)
    ;; redefine some of comint-mode's existing bindings
    (define-key keymap (kbd "C-c C-z") #'shx-send-stop)
    (define-key keymap (kbd "C-c C-d") #'shx-send-eof)
    (define-key keymap (kbd "C-c C-c") #'shx-send-break)
    (define-key keymap (kbd "C-c C-b") #'shx-browse-urls)
    (define-key keymap (kbd "<return>") #'shx-send-input-or-open-thing)
    (define-key keymap (kbd "C-<return>") #'shx-send-input-or-copy-line)
    (define-key keymap (kbd "C-c C-k") #'comint-kill-subjob)
    keymap))

(defun shx-send-input-or-open-thing ()
  "Open thing at point, or send input if no identifiable thing."
  (interactive)
  (if (shx-point-on-input?)
      (shx-send-input)
    (find-file-at-point)))

(defun shx-send-input-or-copy-line ()
  "Copy current line to prompt, or send input if at the prompt."
  (interactive)
  (if (shx-point-on-input?)
      (shx-send-input)
    (let ((line (buffer-substring-no-properties
                 (point-at-bol) (point-at-eol))))
      (goto-char (point-max))
      (insert line))))

(defun shx-send-input ()
  "Send or parse the input currently written at the prompt.
In normal circumstances this input is additionally filtered by
\\[shx-filter-input] via \\[comint-mode]."
  (interactive)
  ;; auto-switch to insert mode
  (and (featurep 'evil-vars)
       (equal evil-state 'normal)
       (featurep 'evil-commands)
       (evil-insert 1))
  (if (> (length (shx--current-input)) 1000)
      (message "Input too long (shorten to < 1000 chars)")
    ;; timestamp the previous prompt
    (unless comint-prompt-read-only
      (add-text-properties
       (let ((inhibit-field-text-motion t)) (point-at-bol))
       (process-mark (get-buffer-process (current-buffer)))
       `(help-echo ,(format-time-string "At %X"))))
    (comint-send-input nil t)))

(defun shx-filter-input (process input)
  "Before sending to PROCESS, filter the INPUT.
That means, if INPUT is a shx-command, do that command instead.
This function overrides \\[comint-input-sender]."
  (let* ((match (string-match (concat "^" shx-leader shx-cmd-syntax) input))
         (shx-cmd (and match (shx--get-user-cmd (match-string 1 input)))))
    (if (not shx-cmd)
        (comint-simple-send process input)
      (funcall shx-cmd (match-string 2 input))
      (with-current-buffer (process-buffer process)
        ;; advance the process mark to trick comint-mode
        (set-marker (process-mark process) (point)))
      ;; send a blank to fetch a new prompt
      (comint-send-string process "\n"))))


;;; output

(defun shx-textwrap (&optional cols)
  "Enable textwrap at COLS columns.
Emacs is especially bad at handling long lines; sometimes
enabling this can provide a significant performance boost."
  (interactive)
  (setq-local fill-column (or cols 80))
  (setq-local adaptive-fill-regexp nil)            ; necessary!
  (setq-local adaptive-fill-first-line-regexp nil) ; faster
  (add-hook 'comint-output-filter-functions #'shx-fill-paragraph nil 'local))

(defun shx-fill-paragraph (_str)
  "Fill (justify) text from the host.
Apply this justification from `comint-last-output-start' to the
buffer's `process-mark'."
  (save-excursion
    (let ((start-of-prompt (point-marker)))
      (goto-char comint-last-output-start)
      (while (< (point) start-of-prompt)
        (let ((region-start (point-at-bol)))
          (forward-line)
          (fill-region region-start (point) nil t))))))

(defun shx-parse-output-hook (&optional _output)
  "Hook to parse the output stream."
  ;; FIXME: these can get expensive on buffers w/ more than 9000 lines
  (shx--parse-output-for-markup)
  (when shx-triggers (shx--parse-output-for-triggers)))

(defun shx--parse-output-for-markup ()
  "Look for markup since `comint-last-output'."
  (save-excursion
    (shx--goto-last-input-or-output)
    (let ((originating-buffer shx-buffer))
      (while (shx--search-forward shx-markup-syntax)
        (let ((command (shx--get-user-cmd (match-string 1)))
              (args    (match-string 2)))
          (if (not (and command (shx--safe-as-markup? command)))
              (add-text-properties
               (point-at-bol) (point-at-eol)
               `(help-echo "shx: this markup was unsafe/undefined"))
            (replace-match "")          ; hide the markup
            (funcall command args)
            (set-buffer originating-buffer)
            ;; some shx commands might add extra newline:
            (and (zerop (current-column))
                 (not (eq 1 (point)))
                 (delete-char 1))))))))

(defun shx--parse-output-for-triggers ()
  "Look for triggers since `comint-last-output' (e.g., URLs)."
  (dolist (trigger shx-triggers nil)
    (save-excursion
      (shx--goto-last-input-or-output)
      (let ((originating-buffer shx-buffer))
        (while (shx--search-forward (car trigger))
          ;; FIXME: emacs 25 had/has a bug where save-window-excursion moves the
          ;; point backward in the calling buffer (some funcalls might use
          ;; save-window-excursion) which can cause infinite triggering.  For
          ;; now, handle this by wrapping the funcall in save-excursion.
          (save-excursion (funcall (cdr trigger)))
          (set-buffer originating-buffer))))))

(defun shx--goto-last-input-or-output ()
  "Go to the beginning of the next event from the process."
  ;; sometimes `comint-last-output-start' is too far back, so
  ;; go to `comint-last-input-end' when that's the case.
  (goto-char (max comint-last-output-start comint-last-input-end))
  (forward-line 0))

(defun shx--search-forward (pattern)
  "Search forward in the past for PATTERN."
  (and (< (line-number-at-pos (point))
          (line-number-at-pos (point-max)))
       (re-search-forward pattern nil t)))


;;; util

(defun shx-browse-urls ()
  "Prompt user for a URL to browse from the list `shx-urls'."
  (interactive)
  (let ((urls shx-urls)) ; clone url list so user edits don't modify the entries
    (browse-url (completing-read (format "URLs in %s: " (buffer-name)) urls))))

(defun shx-describe-command (shx-command)
  "Try to describe the named SHX-COMMAND."
  (let* ((prefix (concat shx-cmd-prefix shx-command))
         (completions (all-completions prefix obarray 'functionp)))
    (describe-function
     (intern
      (if (= (length completions) 1)
          (car completions)
        (completing-read "Describe shx command: " completions nil t prefix))))))

(defun shx-point-on-input? ()
  "Check if point is on the input region."
  (>= (point-marker)
      (process-mark (get-buffer-process (current-buffer)))))

(defun shx--safe-as-markup? (command)
  "Return t if COMMAND is safe to call to generate markup.
In particular whether \"(SAFE)\" prepends COMMAND's docstring."
  (let ((doc (documentation command)))
    (ignore-errors (string= "(SAFE)" (substring doc 0 6)))))

(defun shx--current-prompt ()
  "Return text from start of line to current `process-mark'."
  (cond ((get-buffer-process (current-buffer))
         (save-excursion
           (goto-char (point-max))
           (let ((inhibit-field-text-motion t))
             (buffer-substring-no-properties
              (point-at-bol)
              (process-mark (get-buffer-process (current-buffer)))))))
        (t (message "There is no process.") "")))

(defun shx--current-input ()
  "Return what's written after the prompt."
  (buffer-substring (process-mark (get-buffer-process (current-buffer)))
                    (point-at-eol)))

(defun shx--get-user-cmd (shx-command)
  "Return user command prefixed by SHX-COMMAND, or nil."
  (let* ((prefix (format "%s%s" shx-cmd-prefix (downcase shx-command)))
         (completion (try-completion prefix obarray 'functionp)))
    (when completion
      (intern (if (eq completion t) prefix completion)))))

(defun shx--parse-url ()
  "Add a matched URL to `shx-urls' and make it clickable."
  (let ((url (match-string-no-properties 0)))
    (unless (string= url (car shx-urls)) (push url shx-urls)))
  (add-text-properties
   (match-beginning 0) (match-end 0)
   `(keymap ,shx-click mouse-face link font-lock-face font-lock-doc-face)))

(defun shx--parse-filenames (files)
  "Turn a string of FILES into a list of filename strings.
FILES can have various styles of quoting and escaping."
  (let* ((tmp-space "")
         (requoted (replace-regexp-in-string "'" "\"" files))
         (escaped (replace-regexp-in-string "\\\\ " tmp-space requoted)))
    (mapcar (lambda (filename)
              (replace-regexp-in-string tmp-space " " filename))
            (split-string-and-unquote escaped))))

(defun shx--get-timer-list ()
  "Get the list of resident timers."
  (let ((timer-list-1 (copy-sequence timer-list)))
    ;; select only timers with shx--auto prefix, "(lambda nil (shx--auto..."
    (setq timer-list-1
          (remove nil
                  (mapcar (lambda (timer)
                            (when (string-prefix-p
                                   "(lambda nil (shx--auto"
                                   (format "%s" (aref timer 5)))
                              timer))
                          timer-list-1)))
    ;; sort the timers for consistency
    (sort timer-list-1
          (lambda (first-timer second-timer)
            (string< (format "%s" (aref first-timer 5))
                     (format "%s" (aref second-timer 5)))))))


;;; sending/inserting

(defun shx-send-or-insert ()
  "When the prompt is a colon, send the key pressed, else insert it.
Useful for paging through less."
  (interactive)
  (let ((on-last-line (shx-point-on-input?)))
    (if (and on-last-line
             (string-match ".*:$" (shx--current-prompt))
             (string-match "^\\s-*$" (shx--current-input)))
        (progn
          (message "shx: sending %s" (this-command-keys))
          (process-send-string nil (this-command-keys)))
      (unless on-last-line (goto-char (point-max)))
      (self-insert-command 1))))

(defun shx-send-break ()
  "Send a BREAK signal to the current process."
  (interactive)
  (process-send-string nil ""))

(defun shx-send-eof ()
  "Send an EOF signal to the current process."
  (interactive)
  (if (not (get-buffer-process (current-buffer)))
      (kill-buffer) ; if there's no process, kill the buffer
    (if (string= (shx--current-input) "")
        (process-send-string nil "")
      (goto-char (point-max))
      (comint-kill-input))))

(defun shx-send-stop ()
  "Send a STOP signal to the current process."
  (interactive)
  (process-send-string nil ""))

(defun shx-cat (&rest args)
  "Like \\[concat] but ARGS can be strings or face names."
  (let ((string "")
        (face nil))
    (dolist (arg args nil)
      (cond ((stringp arg)
             (setq string (concat string (propertize arg 'font-lock-face face))))
            ((facep arg)
             (setq face arg))))
    string))

(defun shx-insert (&rest args)
  "Insert ARGS, combined using \\[shx-cat]."
  (insert (apply 'shx-cat args)))

(defun shx-insert-filenames (&rest files)
  "Insert FILES, propertized to be clickable."
  (shx-insert 'font-lock-doc-face
              (mapconcat
               (lambda (file)
                 (propertize file 'keymap shx-click 'mouse-face 'link))
               files "\n")))

(defun shx-insert-timer-list ()
  "Insert a list of the Emacs timers currently in effect."
  (let ((sorted-timer-list (shx--get-timer-list)))
    (dotimes (timer-number (length sorted-timer-list))
      (shx--insert-timer timer-number (nth timer-number sorted-timer-list))
      (shx-insert "\n"))
    (shx-insert 'font-lock-doc-face
                (format "Active timers: %d\n" (length sorted-timer-list)))))

(defun shx-insert-image (filename)
  "Insert image FILENAME into the buffer."
  (let* ((img-name (make-temp-file "tmp" nil ".png"))
         (status (call-process
                  shx-path-to-convert nil t nil (expand-file-name filename)
                  "-resize" (format "x%d>" shx-img-height) img-name)))
    (when (zerop status)
      (let ((pos (point)))
        (insert-image (create-image img-name))
        (add-text-properties pos (point) `(help-echo ,filename)))))
  (shx-insert "\n"))

(defun shx-insert-plot (filename plot-command line-style)
  "Prepare a plot of the data in FILENAME.
Use a gnuplot specific PLOT-COMMAND (for example 'plot') and
LINE-STYLE (for example 'w lp'); insert the plot in the buffer."
  (let ((img-name (make-temp-file "tmp" nil ".png")))
    (when (zerop (call-process shx-path-to-gnuplot nil t nil "-e"
                               (concat "set term png transparent truecolor;"
                                       "set border lw 3 lc rgb \""
                                       (face-attribute 'default :foreground) "\";"
                                       "set out \"" img-name "\"; "
                                       plot-command " \""
                                       (expand-file-name filename) "\" "
                                       line-style)))
      (shx-insert-image img-name))))

(defun shx--insert-timer (timer-number timer)
  "Insert a line of the form '<TIMER-NUMBER> <TIMER>'."
  (shx-insert (format "%d.\s%s" timer-number (shx--format-timer-string timer))
              (when (aref timer 4) (format "\s(pulse: %d)" (aref timer 4)))))

(defun shx--format-timer-string (timer)
  "Create a human-readable string out of TIMER."
  (let ((timer-string (format "%s" (aref timer 5))))
    (substring timer-string 23 (- (length timer-string) 2))))


;;; asynch functions

(defun shx--asynch-funcall (function &optional args)
  "Run FUNCTION with ARGS in the buffer after a short delay."
  (run-at-time "0.2 sec" nil
               `(lambda ()
                  (with-current-buffer ,shx-buffer ,(cons function args)))))

(defun shx--delay-input (delay input &optional buffer repeat-interval)
  "After DELAY, process INPUT in the BUFFER.
If BUFFER is nil, process in the current buffer.  Optional
REPEAT-INTERVAL specifies delays between repetitions."
  (let* ((process (get-buffer-process (buffer-name buffer)))
         (funcall `(lambda () ,(cons 'shx--auto (list process input)))))
    (run-at-time delay repeat-interval funcall)))

(defun shx--auto (process command)
  "Send PROCESS a COMMAND.
This cosmetic function only exists to make the listing generated
by \\[shx-insert-timer-list] easier to parse."
  (process-send-string process (concat command "\n")))


;;; asynch user commands

(defun shx-cmd/delay (args)
  "Run a command after a specific delay.
ARGS are <delay in seconds> <command>.
Cancel with :stop.
Example:
  :delay 10 echo Ten seconds are up!"
  (cond
   ((string-match "^\\([0-9.]+\\)\\s-+\\(.+\\)$" args)
    (let ((delay (match-string 1 args))
          (command (match-string 2 args)))
      (shx-insert "Delaying " 'comint-highlight-input command
                  'default (format " %s seconds\n" delay))
      (shx--delay-input (concat delay " sec") command)))
   (t (shx-insert 'error "delay <delay> <command>\n"))))

(defun shx-cmd/pulse (args)
  "Repeat a shell command indefinitely with a given delay.
ARGS are <deley in seconds> <command>.
Cancel with :stop.
Example:
  :pulse 10 date"
  (cond
   ((string-match "^\\([0-9.]+\\)\\s-+\\(.+\\)$" args)
    (let ((delay (string-to-number (match-string 1 args)))
          (command (match-string 2 args)))
      (shx-insert "Pulsing " 'comint-highlight-input command
                  'default (format " every %d seconds\n" delay))
      (shx--delay-input 0 command nil delay)))
   (t (shx-insert 'error "pulse <delay> <command>\n"))))

(defun shx-cmd/repeat (args)
  "Repeat a shell command a number of times with a given delay.
ARGS are <count> <delay in seconds> <command>.
Use :stop to cancel.
Example:
  :repeat 3 1 echo Echo... echo... echo..."
  (cond
   ((string-match "^\\([0-9]+\\)\\s-+\\([0-9.]+\\)\\s-+\\(.+\\)$" args)
    (let ((reps (string-to-number (match-string 1 args)))
          (delay (string-to-number (match-string 2 args)))
          (command (match-string 3 args)))
      (shx-insert "Repeating " 'comint-highlight-input command 'default
                  (format " %d times every %d seconds\n" reps delay))
      (dotimes (ii reps)
        (shx--delay-input (* (1+ ii) delay) command))))
   (t (shx-insert 'error "repeat <count> <delay> <command>\n"))))

(defun shx-cmd/stop (timer-number)
  "(SAFE) Enumerate all resident timers.
If a TIMER-NUMBER is supplied, cancel the specified timer.
Examples:
  :stop
  :stop 3"
  (let ((timer-number-int (string-to-number timer-number)))
    (unless (or (> timer-number-int (1- (length timer-list)))
                (not (equal (int-to-string timer-number-int) timer-number)))
      (let ((timer (nth timer-number-int (shx--get-timer-list))))
        (shx-insert "Stopping " 'font-lock-keyword-face
                    (shx--format-timer-string timer) "\n")
        (cancel-timer timer))))
  (shx-insert-timer-list))


;;; general user commands

(defun shx-cmd/alert (string)
  "(SAFE) Show the shx-buffer in the other window with STRING."
  (message (format "From %s at %s: '%s'\n"
                   shx-buffer
                   (format-time-string "%X")
                   string))
  (display-buffer shx-buffer))

(defun shx-cmd/clear (_args)
  "(SAFE) Clear the buffer.
Fails if there are read-only elements such as a prompt -
therefore ensure `comint-prompt-read-only' is nil."
  (erase-buffer))

(defun shx-cmd/date (_args)
  "(SAFE) Show the date."
  (shx-insert (current-time-string) "\n"))

(defun shx-cmd/diff (files)
  "(SAFE) Launch an Emacs \\[ediff] between FILES.
Example:
:diff file1.txt file2.csv"
  (shx-insert "Invoking ediff " files "\n")
  (shx--asynch-funcall
   'ediff (mapcar 'expand-file-name (shx--parse-filenames files))))

(defun shx-cmd/edit (file)
  "(SAFE) open FILE in the current window.
Example:
:e directory/to/file
Or, to edit a remote file using tramp:
:e /username@server:directory/to/file"
  (if (equal file "")
      (shx--asynch-funcall #'find-file (list "" t))
    (let ((name (expand-file-name (car (shx--parse-filenames file)))))
      (shx--asynch-funcall #'find-file (list name t)))))
(defalias 'shx-cmd/e #'shx-cmd/edit)

(defun shx-cmd/find (file)
  "(SAFE) run fuzzy find for FILE."
  (if (equal file "")
      (shx-insert 'error "find <prefix>\n")
    (let* ((fuzzy-file (mapconcat 'char-to-string (string-to-list file) "*"))
           (command (format "find . \-iname '%s*'" fuzzy-file))
           (output (shell-command-to-string command)))
      (if (equal "" output)
          (shx-insert 'error "No matches for \"" file "\"\n")
        (apply #'shx-insert-filenames
               (split-string (substring output 0 -1) "\n"))
        (insert "\n")))))

(defun shx-cmd/g (pattern)
  "(SAFE) Launch a recursive grep for PATTERN."
  ;;(grep (format "grep -irnH '%s' *" pattern)))
  (grep (format "ag --noheading -ir '%s'" pattern)))

(defun shx-cmd/grep (pattern)
  "(SAFE) Launch a grep for PATTERN.
Examples:
  :grep -r 'pattern' *
  :grep 'pattern' * | grep -v 'exclusion'"
  (grep (format "grep -nH %s" pattern)))

(defun shx-cmd/header (header)
 "(SAFE) Set the header-line to to HEADER.
See `header-line-format' for formatting options.
Examples:
:header remote:%@  status:%s  size:%i
:header
PS1=\"<header \\$(git status 2>/dev/null |head -1)>\\\\n$PS1\"
PS1=\"<header \\$(git status -s 2>/dev/null|paste -s -d \\\" \\\" - )>\\\\n$PS1\""
  (setq header-line-format
        (and (not (string= header "")) header)))

(defun shx-cmd/help (shx-command)
  "(SAFE) Display help on the SHX-COMMAND.
If function doesn't exist (or none is supplied), read from user."
  (shx--asynch-funcall #'shx-describe-command (list shx-command)))

(defun shx-cmd/eval (sexp)
  "Evaluate the elisp SEXP.
Examples:
:eval (format \"%d\" (+ 1 2))
:eval (* 2 (+ 3 5))"
  (condition-case nil
      (let ((originating-buffer (current-buffer))
            (output (format "%s\n" (eval (car (read-from-string sexp))))))
        (with-current-buffer originating-buffer
          (shx-insert 'font-lock-constant-face "=> " output)))
    (error (shx-insert 'error "invalid sexp\n"))))

(defun shx-cmd/man (topic)
  "(SAFE) Launch an Emacs \\[man] window for TOPIC.
See `Man-notify-method' for what happens when the page is ready."
  (if (equal topic "")
      (shx-insert 'error "man <topic>\n")
    (shx-insert "Invoking 'man " topic "' in other window\n")
    (man topic)))

(defun shx-cmd/name (name)
  "(SAFE) Rename the current buffer to NAME."
  (if (ignore-errors (rename-buffer (concat "*" name "*")))
      (shx-insert "Renaming buffer to *" name "*\n")
    (shx-insert 'error "Can't rename buffer to *" name "* (is this name taken?)\n")))

(defun shx-cmd/oedit (file)
  "(SAFE) open FILE in other window.
Syntax: :o[edit] directory/to/file
Syntax: :o[edit] /username@server:~/directory/to/file"
  (if (equal file "")
      (find-file-other-window "")
    (find-file-other-window
     (expand-file-name (replace-regexp-in-string "\\\\" "" file)))))

(defun shx-cmd/pwd (_args)
  "(SAFE) Show what Emacs thinks the pwd is."
  (shx-insert default-directory "\n"))

(defun shx-cmd/ssh (host)
  "(SAFE) open a shell on (remote) HOST using tramp.
Syntax: :ssh hostname"
  (if (equal host "")
      (shx-insert 'error "ssh host\n")
    (let ((default-directory (concat "/" host ":~")))
      (shx))))


;;; graphical user commands

(defun shx-cmd/plotbar (filename)
  "(SAFE) Show barplot of FILENAME.
Example file contents:
\"Topic 1\" YHEIGHT1
\"Topic 2\" YHEIGHT2
\"Topic 3\" YHEIGHT3"
  (shx-insert-plot filename "set boxwidth 1.5 relative;
                             set style data histograms;
                             set xtic rotate by -40 scale 0 font \",10\";
                             set yrange [0:];
                             set style fill solid 1.0 border -1;
                             plot" "u 2:xticlabels(1) notitle"))
(defalias 'shx-cmd/barplot #'shx-cmd/plotbar) ; TODO: deprecated

(defun shx-cmd/plotmatrix (filename)
  "(SAFE) Show heatmap of FILENAME.
Example file contents:
1.5   2    3
4     5    6
7     8    9.5"
  (shx-insert-plot filename "set view map; unset xtics; unset ytics;
                             unset title; set colorbox; set palette defined
                               (0 \"#ffffff\", 1 \"#d5e585\", 2 \"#8cc555\",
                                3 \"#55a550\", 4 \"#1e5500\");
                             plot" "u 1:(-$2):3 matrix w image notitle"))
(defalias 'shx-cmd/matrix #'shx-cmd/plotmatrix) ; TODO: deprecated

(defun shx-cmd/plotline (filename)
  "(SAFE) Show line plot of FILENAME.
Example file contents 1:
1 2
2 4
4 8
Example file contents 2:
1
2
3
5"
  (shx-insert-plot filename "plot" "w lp lw 1 ps 2 pt 7 notitle"))
(defalias 'shx-cmd/plot #'shx-cmd/plotline) ; TODO: deprecated

(defun shx-cmd/plot3d (filename)
  "(SAFE) Show surface plot of FILENAME.
Read about gnuplot's expectations of the data here:
http://www.gnuplotting.org/tag/pm3d/"
  (shx-insert-plot filename "unset tics;
                             set view 4, 20, 1.4, 1;
                             splot" "w pm3d notitle"))

(defun shx-cmd/plotscatter (filename)
  "(SAFE) Show scatter plot of FILENAME.
Example file contents 1:
1 2
2 4
4 8
Example file contents 2:
1
2
3
5"
  (shx-insert-plot filename "plot" "w p ps 2 pt 7 notitle"))
(defalias 'shx-cmd/scatter #'shx-cmd/plotscatter) ; TODO: deprecated

(defun shx-cmd/view (filename)
  "(SAFE) View image with FILENAME directly in the buffer."
  (shx-insert-image filename))


;;; loading

(defvar shx-keymap (shx-get-keymap)
  "To self-document the shx/shx-active functions.")

;; Add hooks and advise some existing comint functions:
(when shx-auto-run (add-hook 'comint-mode-hook 'shx-activate))
(add-hook 'shell-mode-hook 'shx-for-shell-mode) ; always run in shell-mode
(advice-add 'comint-send-eof
            :before (lambda () (goto-char (point-max))))
(advice-add 'comint-history-isearch-backward-regexp
            :before (lambda () (goto-char (point-max))))
(advice-add 'comint-previous-input
            :before (lambda (arg) (goto-char (point-max))))
(advice-add 'comint-next-input
            :before (lambda (arg) (goto-char (point-max))))
(advice-add 'comint-previous-prompt
            :after (lambda (arg) (recenter-top-bottom 0)))
(advice-add 'comint-next-prompt
            :after (lambda (arg) (recenter-top-bottom 0)))

(defun shx-activate ()
  "Activate shx on the current buffer.
shx provides the following key bindings:
\n\\{shx-keymap}"
  (interactive)
  (use-local-map (shx-get-keymap (current-local-map)))
  (buffer-disable-undo)
  (setq-local shx-urls (list "https://github.com/riscy/shx-for-emacs"))
  (setq-local shx-buffer (current-buffer))
  (setq comint-prompt-read-only nil)
  (add-hook 'comint-output-filter-functions #'shx-parse-output-hook nil 'localy)
  ;; do this asynch with a delay; spacemacs tries to set this variable too
  (shx--asynch-funcall (lambda () (setq comint-input-sender 'shx-filter-input))))

(defun shx-for-shell-mode ()
  "Active shx in the context of \\[shell-mode].
For this function to work properly, it should be in `shell-mode-hook'."
  (setq-local shell-font-lock-keywords
              (append shell-font-lock-keywords shx-shell-mode-font-locks))
  (setq-local font-lock-defaults '(shell-font-lock-keywords t))
  (shx-activate))

(defun shx (&optional name)
  "Create a new shell session using shx.
NAME is the optional name of the buffer.
shx provides the following key bindings:
\n\\{shx-keymap}"
  (interactive)
  (let ((name (or name (generate-new-buffer-name "*shx*"))))
    ;; switch-to-buffer first -- shell uses pop-to-buffer
    ;; which is unpredictable! :(
    (switch-to-buffer name)
    (shell name)))

(provide 'shx)
;;; shx ends here
