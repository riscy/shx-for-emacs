;;; shx.el --- "Extras" for the (comint-mode) shell

;; Authors: Chris Rayner (dchrisrayner @ gmail)
;; Created: May 23 2011
;; Keywords: processes, tools
;; URL: https://github.com/riscy/shx-for-emacs
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.0.10

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; shx or "shell-extras" extends comint-mode.  It parses simple markup in the
;; output stream (enabling plots and graphics to be embedded in the shell) and
;; adds several command-line functions which plug into Emacs (for example, use
;; :e <filename> to edit a file).
;;
;; See <https://github.com/riscy/shx-for-emacs/blob/master/README.org> for more.
;;
;; This version tested with Emacs 25.2.1

;;; Manual install:

;; 1. Move shx.el to a directory in your load-path or add
;;    this to your .emacs:
;;    (add-to-list 'load-path "~/path/to/this-file/")
;; 2. Next add this line to your .emacs:
;;    (require 'shx)
;;
;; By default, shx runs automatically in all comint-mode buffers, but you
;; can always use M-x shx RET to create a new shell session using shx.
;;
;; Use M-x customize-group RET shx RET to see customization options.

;;; Code:

(require 'color)
(require 'comint)
(require 'shell)
(require 'subr-x)

;; Compiler pacifier
(defvar evil-state)
(declare-function evil-insert-state "evil-states")


;;; customization options and other variables

(defgroup shx nil
  "Extras for the (comint-mode) shell."
  :prefix "shx-"
  :group 'comint
  :link '(url-link
          :tag "shx on GitHub"
          "https://github.com/riscy/shx-for-emacs"))

(defcustom shx-disable-undo nil
  "Whether to disable undo in shx buffers."
  :type 'boolean)

(defcustom shx-path-to-convert "convert"
  "Path to ImageMagick's convert binary."
  :type 'string)

(defcustom shx-mode-lighter "shx"
  "Lighter for the shx minor mode."
  :type 'string)

(defcustom shx-path-to-gnuplot "gnuplot"
  "Path to gnuplot binary."
  :type 'string)

(defcustom shx-img-height 300
  "The height at which inlined images and plots are displayed."
  :type 'integer)

(defcustom shx-use-magic-insert t
  "Whether to dynamically modify input using `shx-magic-insert'."
  :link '(function-link shx-magic-insert)
  :type 'boolean)

(defcustom shx-leader ":"
  "Prefix for calling user commands."
  :type 'regexp)

(defcustom shx-comint-advise t
  "Whether to advise the behavior of a number of `comint-mode' functions."
  :type 'boolean)

(defcustom shx-show-hints t
  "Whether to echo hints when running certain commands."
  :type 'boolean)

(defcustom shx-triggers
  '(("https?://[A-Za-z0-9,./?=&;_-]+[^.\n\s\"'>)]+" . shx--parse-url))
  "Triggers of the form: (regexp . function)."
  :type '(alist :key-type regexp :value-type function))

(defcustom shx-kept-commands
  '(("Enable wordwrap at 90 columns" . ":eval (shx-wordwrap 90)"))
  "Shell commands of the form (description . command)."
  :link '(function-link shx-cmd-kept)
  :link '(function-link shx-cmd-keep)
  :type '(alist :key-type string :value-type string))

(defvar shx-cmd-prefix "shx-cmd-"
  "Prefix for user-command functions.")

(defvar shx-cmd-syntax "\\([^[:space:]]+\\)[[:space:]]*\\(.*[^[:space:]]?\\)"
  "Regex for recognizing shx commands in input or markup.")

(defvar shx-markup-syntax (concat "^<" shx-cmd-syntax ">$")
  "Regex for recognizing shx commands in markup.")

(defvar shx-mode-map
  (let ((keymap (make-sparse-keymap)))
    (when shx-use-magic-insert
      (define-key keymap " " #'shx-magic-insert)
      (define-key keymap "q" #'shx-magic-insert))
    (define-key keymap (kbd "<return>") #'shx-send-input-or-open-thing)
    (define-key keymap (kbd "C-<return>") #'shx-send-input-or-copy-line)
    keymap)
  "Keymap for shx.")

(defvar-local shx-buffer nil
  "Local reference to the shx buffer.")

(defvar-local shx-prompt-overlay nil
  "Overlay used to highlight the prompt.")

(defvar-local shx-urls nil
  "Local record of URLs seen.")

(defvar shx-click-file (let ((keymap (make-sparse-keymap)))
                         (define-key keymap [mouse-1] 'ffap-at-mouse)
                         keymap)
  "Keymap for capturing mouse clicks on files/URLs.")

(defvar-local shx--old-prompt-read-only nil
  "Whether the prompt was read-only before shx-mode was enabled.")

(defvar-local shx--old-undo-disabled nil
  "Whether undo was disabled before shx-mode was enabled.")


;;; input

(defun shx-send (command)
  "Insert and send COMMAND as if the user had done so.
This can help in running `ibuffer-do-eval' on multiple buffers."
  (comint-kill-input)
  (insert command)
  (shx-send-input))

(defun shx-send-input-or-open-thing ()
  "Open thing at point, or send input if no identifiable thing."
  (interactive)
  (if (shx-point-on-input-p)
      (shx-send-input)
    (find-file-at-point)))

(defun shx-send-input-or-copy-line ()
  "Copy current line to prompt, or send input if at the prompt."
  (interactive)
  (if (shx-point-on-input-p)
      (shx-send-input)
    (let ((line (buffer-substring-no-properties
                 (point-at-bol) (point-at-eol))))
      (goto-char (point-max))
      (insert line))))

(defun shx-send-input ()
  "Send or parse the input currently written at the prompt.
In normal circumstances this input is additionally filtered by
`shx-filter-input' via `comint-mode'."
  (interactive)
  (if (> (length (shx--current-input)) 1000)
      (message "Input too long (shorten to < 1000 chars)")
    (shx--timestamp-prompt)
    (comint-send-input nil t)))

(defun shx-filter-input (process input)
  "Before sending to PROCESS, filter the INPUT.
That means, if INPUT is a shx-command, do that command instead.
This function overrides `comint-input-sender'."
  (let* ((match (string-match (concat "^" shx-leader shx-cmd-syntax)
                              (string-trim-left input)))
         (shx-cmd (and match (shx--get-user-cmd (match-string 1 input)))))
    (if (not shx-cmd)
        (comint-simple-send process input)
      (funcall shx-cmd (match-string 2 input))
      (with-current-buffer (process-buffer process)
        ;; advance the process mark to trick comint-mode
        (set-marker (process-mark process) (point)))
      ;; send a blank to fetch a new prompt
      (comint-send-string process "\n"))))

(defun shx--timestamp-prompt ()
  "Add a mouseover timestamp to the last prompt."
  (unless comint-prompt-read-only
    (add-text-properties
     (let ((inhibit-field-text-motion t)) (point-at-bol))
     (process-mark (get-buffer-process (current-buffer)))
     `(help-echo ,(format-time-string "At %X")))))


;;; output

(defun shx-wordwrap (&optional cols)
  "Enable wordwrap at COLS columns.
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
          (if (not (and command (shx--safe-as-markup-p command)))
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
          ;; emacs 25 had/has a bug where save-window-excursion moves the point
          ;; backward in the calling buffer (some funcalls might use
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
  "Search forward from the current point for PATTERN."
  (when (< (point-at-eol) (point-max))
    (re-search-forward pattern nil t)))


;;; util

(defun shx-browse-urls ()
  "Prompt user for a URL to browse from the list `shx-urls'."
  (interactive)
  (let ((urls shx-urls)) ; clone url list so user edits don't modify the entries
    (browse-url (completing-read "URL: " urls))))

(defun shx-describe-command (shx-command)
  "Try to describe the named SHX-COMMAND."
  (let ((prefix (concat shx-cmd-prefix shx-command)))
    (if (functionp (intern prefix))
        (describe-function (intern prefix))
      (let ((comp (completing-read "Complete shx command: "
                                   (shx--all-commands) nil t prefix)))
        (describe-function (intern comp))))))

(defun shx-point-on-input-p ()
  "Check if point is on the input region."
  (let ((process (get-buffer-process (current-buffer))))
    (and process (>= (point-marker) (process-mark process)))))

(defun shx--all-commands (&optional without-prefix)
  "Return a list of all shx commands.
With non-nil WITHOUT-PREFIX, strip `shx-cmd-prefix' from each."
  (mapcar (lambda (cmd)
            (if without-prefix (string-remove-prefix shx-cmd-prefix cmd) cmd))
          (all-completions shx-cmd-prefix obarray 'functionp)))

(defun shx--escape-filename (filename)
  "Escape FILENAME to mitigate injection attacks."
  (replace-regexp-in-string ; modeled on Ruby's "Shellwords"
   "\\([^A-Za-z0-9_\-.,:\/@\n]\\)" "\\\\\\1"
   (expand-file-name filename)))

(defun shx--hint (text)
  "Show a hint containing TEXT."
  (when shx-show-hints (message (concat "Hint: " text))))

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

(defun shx--get-user-cmd (cmd-prefix)
  "Return user command prefixed by CMD-PREFIX, or nil."
  (let* ((prefix (format "%s%s" shx-cmd-prefix (downcase cmd-prefix)))
         (completion (try-completion prefix obarray 'functionp)))
    (when completion
      (let ((user-cmd (intern (if (eq completion t) prefix completion))))
        (when (functionp user-cmd) user-cmd)))))

(defun shx--parse-url ()
  "Add a matched URL to `shx-urls' and make it clickable."
  (let ((url (match-string-no-properties 0)))
    (unless (string= url (car shx-urls)) (push url shx-urls)))
  (add-text-properties
   (match-beginning 0) (match-end 0)
   `(keymap ,shx-click-file mouse-face link font-lock-face font-lock-doc-face)))

(defun shx-tokenize (string)
  "Turn STRING into a list of tokens, or nil if parsing fails.
This is robust to various styles of quoting and escaping."
  (let* ((tmp-space "")
         (requoted (replace-regexp-in-string "'" "\"" string))
         (escaped (replace-regexp-in-string "\\\\ " tmp-space requoted)))
    (mapcar (lambda (token)
              (replace-regexp-in-string tmp-space " " token))
            (ignore-errors (split-string-and-unquote escaped)))))

(defun shx--quote-regexp (delimiter &optional escape max-length)
  "Regexp matching strings delimited by DELIMITER.
ESCAPE is the string that can be used to escape the delimiter.
MAX-LENGTH is the length of the longest match (default 300)."
  (concat delimiter
          "\\("
          (when escape
            (concat escape escape "\\|"      ; two backslashes OR
                    escape delimiter "\\|")) ; escaped delimiter
          "[^" delimiter "]"
          "\\)"
          "\\{0," (format "%d" (or max-length 300)) "\\}"
          delimiter))

(defun shx--safe-as-markup-p (command)
  "Return t if COMMAND is safe to call to generate markup.
In particular whether \"(SAFE)\" prepends COMMAND's docstring."
  (let ((doc (documentation command)))
    (ignore-errors (string-prefix-p "(SAFE)" doc))))

(defun shx--restore-kept-commands (&optional regexp insert-kept-command)
  "Add commands from `shx-kept-commands' into `comint-input-ring'.
REGEXP filters which commands to add.  If INSERT-KEPT-COMMAND is
not nil, then insert the command into the current buffer."
  (dolist (command shx-kept-commands nil)
    (when (string-match (or regexp ".") (concat (car command) (cdr command)))
      (when insert-kept-command
        (shx-insert 'font-lock-doc-face (car command) 'default ": "
                    'comint-highlight-input command (cdr command) "\n"))
      (ring-insert comint-input-ring (cdr command)))))


;;; sending/inserting

(defun shx-magic-insert ()
  "Insert the key pressed or dynamically change the input.
`comint-magic-space' completes substitutions like '!!' and
'^pattern^replacement', and if the prompt is a colon, SPC and q
are sent straight through to the process to handle paging."
  (interactive)
  (let ((on-last-line (shx-point-on-input-p)))
    (if (and on-last-line
             (string-match ".*:$" (shx--current-prompt))
             (string-match "^\\s-*$" (shx--current-input)))
        (progn
          (message "shx: sending '%s'" (this-command-keys))
          (process-send-string nil (this-command-keys)))
      (unless on-last-line (goto-char (point-max)))
      (if shx-use-magic-insert
          (comint-magic-space 1)
        (self-insert-command 1)))))

(defun shx-cat (&rest args)
  "Like `concat' but ARGS can be strings or face names."
  (let ((string "")
        (face nil))
    (dolist (arg args nil)
      (cond ((stringp arg)
             (setq string (concat string (propertize arg 'font-lock-face face))))
            ((facep arg)
             (setq face arg))))
    string))

(defun shx-insert (&rest args)
  "Insert ARGS, combined using `shx-cat'."
  (insert (apply 'shx-cat args)))

(defun shx-insert-filenames (&rest files)
  "Insert FILES, propertized to be clickable."
  (shx-insert 'font-lock-doc-face
              (mapconcat
               (lambda (file)
                 (propertize file 'keymap shx-click-file 'mouse-face 'link))
               files "\n")))

(defun shx-insert-timer-list ()
  "Insert a list of the Emacs timers currently in effect."
  (let ((sorted-timer-list (shx--get-timer-list)))
    (dotimes (timer-number (length sorted-timer-list))
      (shx--insert-timer (+ 1 timer-number) (nth timer-number sorted-timer-list))
      (shx-insert "\n"))
    (shx-insert "Active timers: " 'font-lock-constant-face
                (format "%d\n" (length sorted-timer-list)))))

(defun shx-insert-image (filename)
  "Insert image FILENAME into the buffer."
  (let* ((img-name (make-temp-file "tmp" nil ".png"))
         (status (call-process
                  ;; NOTE: FILENAME is interpreted literally by emacs and
                  ;; does not need to go through shx--escape-filename:
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
  (let* ((img-name (make-temp-file "tmp" nil ".png"))
         (status (call-process
                  shx-path-to-gnuplot nil t nil "-e"
                  (concat
                   "set term png transparent truecolor;"
                   "set border lw 3 lc rgb \""
                   (color-lighten-name (face-attribute 'default :foreground) 5)
                   "\"; set out \"" img-name "\"; "
                   plot-command " \""
                   (shx--escape-filename filename) "\" "
                   line-style))))
    (when (zerop status) (shx-insert-image img-name))))

(defun shx--insert-timer (timer-number timer)
  "Insert a line of the form '<TIMER-NUMBER> <TIMER>'."
  (shx-insert
   'font-lock-constant-face (format "%d" timer-number)
   'font-lock-string-face (format ". %s" (shx--format-timer-string timer))
   (when (aref timer 4) (format "\s(pulse: %d)" (aref timer 4)))))

(defun shx--format-timer-string (timer)
  "Create a human-readable string out of TIMER."
  (let* ((timer-string (string-remove-prefix
                        "(lambda nil (shx--auto "
                        (string-remove-suffix
                         "))"
                         (format "%s" (aref timer 5))))))
    (concat "[" timer-string "]")))


;;; asynch functions

(defun shx--asynch-funcall (function &optional args delay)
  "Run FUNCTION with ARGS in the buffer after a short DELAY."
  (run-at-time (or delay 0.2) nil
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
\(Makes the `shx-insert-timer-list' listing easier to parse.\)"
  (process-send-string process (concat command "\n")))


;;; asynch user commands

(defun shx-cmd-delay (args)
  "Run a command after a specific delay.
ARGS are <delay in seconds> <command>.
Cancel a delayed command with :stop (`shx-cmd-stop').
\nExample:\n
  :delay 10 echo Ten seconds are up!"
  (cond
   ((string-match "^\\([0-9.]+\\)\\s-+\\(.+\\)$" args)
    (let ((delay (match-string 1 args))
          (command (match-string 2 args)))
      (shx-insert "Delaying " 'comint-highlight-input command
                  'default (format " %s seconds\n" delay))
      (shx--delay-input (concat delay " sec") command))
    (shx--hint "cancel a delayed command with :stop"))
   (t (shx-insert 'error "delay <delay> <command>\n"))))

(defun shx-cmd-pulse (args)
  "Repeat a shell command indefinitely with a given delay.
ARGS are <deley in seconds> <command>.
Cancel a pulsing command with :stop (`shx-cmd-stop').
\nExample:\n
  :pulse 10 date"
  (cond
   ((string-match "^\\([0-9.]+\\)\\s-+\\(.+\\)$" args)
    (let ((delay (string-to-number (match-string 1 args)))
          (command (match-string 2 args)))
      (shx-insert "Pulsing " 'comint-highlight-input command
                  'default (format " every %d seconds\n" delay))
      (shx--delay-input 0 command nil delay))
    (shx--hint "cancel a pulsing command with :stop"))
   (t (shx-insert 'error "pulse <delay> <command>\n"))))

(defun shx-cmd-repeat (args)
  "Repeat a shell command a number of times with a given delay.
ARGS are <count> <delay in seconds> <command>.
Cancel a repeating command with :stop (`shx-cmd-stop').
\nExample:\n
  :repeat 3 1 echo Echo... echo... echo..."
  (cond
   ((string-match "^\\([0-9]+\\)\\s-+\\([0-9.]+\\)\\s-+\\(.+\\)$" args)
    (let ((reps (string-to-number (match-string 1 args)))
          (delay (string-to-number (match-string 2 args)))
          (command (match-string 3 args)))
      (shx-insert "Repeating " 'comint-highlight-input command 'default
                  (format " %d times every %d seconds\n" reps delay))
      (dotimes (ii reps)
        (shx--delay-input (* (1+ ii) delay) command)))
    (shx--hint "cancel a repeating command with :stop"))
   (t (shx-insert 'error "repeat <count> <delay> <command>\n"))))

(defun shx-cmd-stop (timer-number)
  "(SAFE) Stop the specified shx timer.
If a TIMER-NUMBER is not supplied, enumerate all shx timers.
\nExamples:\n
  :stop
  :stop 3"
  (setq timer-number (1- (string-to-number timer-number)))
  (let ((shx-timer-list (shx--get-timer-list)))
    (and (>= timer-number 0)
         (< timer-number (length shx-timer-list))
         (let ((timer (nth timer-number shx-timer-list)))
           (shx-insert "Stopped " 'font-lock-string-face
                       (shx--format-timer-string timer) "\n")
           (cancel-timer timer))))
  (shx-insert-timer-list))


;;; general user commands

(defun shx-cmd-alert (string)
  "(SAFE) Show the `shx-buffer' in the other window with STRING."
  (message (format "From %s at %s: '%s'\n"
                   shx-buffer
                   (format-time-string "%X")
                   string))
  (display-buffer shx-buffer))

(defun shx-cmd-clear (_args)
  "(SAFE) Clear the buffer.
Fails if there are read-only elements such as a prompt -
therefore ensure `comint-prompt-read-only' is nil."
  (erase-buffer))

(defun shx-cmd-date (_args)
  "(SAFE) Show the date."
  (shx-insert (current-time-string) "\n"))

(defun shx-cmd-diff (files)
  "(SAFE) Launch an Emacs `ediff' between FILES.
\nExample:\n
  :diff file1.txt file2.csv"
  (setq files (shx-tokenize files))
  (if (not (eq (length files) 2))
      (shx-insert 'error "diff <file1> <file2>\n")
    (shx-insert "invoking ediff...\n")
    (shx--asynch-funcall #'ediff (mapcar 'expand-file-name files))))

(defun shx-cmd-edit (file)
  "(SAFE) open FILE in the current window.
\nExamples:\n
  :e directory/to/file
\nOr edit a remote file using `tramp':\n
  :e /user@server#port:directory/to/file"
  (setq file (car (shx-tokenize file)))
  (if file
      (shx--asynch-funcall #'find-file (list (expand-file-name file) t))
    (shx--asynch-funcall #'find-file (list "" t))))
(defalias 'shx-cmd-e #'shx-cmd-edit)

(defun shx-cmd-eval (sexp)
  "Evaluate the elisp SEXP.
\nExamples:\n
  :eval (format \"%d\" (+ 1 2))
  :eval (* 2 (+ 3 5))"
  (condition-case nil
      (let ((originating-buffer (current-buffer))
            (output (format "%s\n" (eval (car (read-from-string sexp))))))
        (with-current-buffer originating-buffer
          (shx-insert 'font-lock-constant-face "=> " output)))
    (error (shx-insert 'error "invalid sexp\n"))))

(defun shx-cmd-find (file)
  "Run fuzzy find for FILE.
Depending on the contents of the current directory, this command
may take a while and unfortunately blocks Emacs in the meantime.
\nExamples:\n
  :f prefix
  :f *suffix"
  (if (equal file "")
      (shx-insert 'error "find <prefix>\n")
    (let* ((fuzzy-file (mapconcat 'char-to-string (string-to-list file) "*"))
           (command (format "find . \-iname '%s*'" fuzzy-file))
           (output (shell-command-to-string command)))
      (if (equal "" output)
          (shx-insert 'error "No matches for \"" file "\"\n")
        (apply #'shx-insert-filenames
               (split-string (string-remove-suffix "\n" output) "\n"))
        (insert "\n")))))

(defun shx-cmd-pipe (command)
  "Pipe the output of COMMAND to a compilation buffer.
\nExamples:\n
  :pipe make
  :pipe git repack -a -d --depth=250 --window=250"
  (if (equal command "")
      (shx-insert 'error "cap <command>\n")
    (let ((compilation-buffer-name-function
           (lambda (_mode) "*shx-pipe*")))
      (shx-insert "Piping "
                  'comint-highlight-input command 'default
                  " to " "*shx-pipe*\n")
      (compile command t))))

(defun shx-cmd-g (pattern)
  "Launch a recursive grep for PATTERN."
  (grep (format "grep -irnH '%s' *" pattern)))

(defun shx-cmd-goto-url (_arg)
  "Go to a a URL, offering completions from the buffer."
  (shx--asynch-funcall #'shx-browse-urls))

(defun shx-cmd-grep (pattern)
  "Launch a grep for PATTERN.
\nExamples:\n
  :grep -r 'pattern' *
  :grep 'pattern' * | grep -v 'exclusion'"
  (grep (format "grep -nH %s" pattern)))

(defun shx-cmd-header (header)
  "(SAFE) Set the header-line to HEADER.
See `header-line-format' for formatting options.
\nExamples:\n
  :header remote:%@  status:%s  size:%i
  :header
\nOr, adding <header ...> in markup form to your prompt:\n
  export PS1=\"<header \\$(git rev-parse --abbrev-ref HEAD)>\\\\n$PS1\"
  export PS1=\"<header \\$(git status -s 2>/dev/null|paste -s -d \\\" \\\" - )>\\\\n$PS1\""
  (setq header-line-format
        (and (not (string-empty-p header)) header)))

(defun shx-cmd-help (shx-command)
  "(SAFE) Display help on the SHX-COMMAND.
If function doesn't exist (or none is supplied), read from user."
  (shx--asynch-funcall #'shx-describe-command (list shx-command)))
(defalias 'shx-cmd-h #'shx-cmd-help)

(defun shx-cmd-keep (_arg)
  "(SAFE) Add the previous command into `shx-kept-commands'.
This enables it to be accessed later using `shx-cmd-kept'."
  (let* ((command (substring-no-properties (ring-ref comint-input-ring 1)))
         (desc (read-string (format "'%s'\nDescription: " command))))
    (if (string-empty-p desc)
        (shx-insert 'error "Description is required\n")
      (add-to-list 'shx-kept-commands `(,desc . ,command))
      (customize-save-variable 'shx-kept-commands shx-kept-commands)
      (shx-insert "Kept as " 'font-lock-doc-face desc "\n")
      (shx--hint "type ':kept' or ':k' to see all kept commands"))))

(defun shx-cmd-kept (regexp)
  "(SAFE) List the \"kept\" commands that match REGEXP.
Each matching command is appended to the input history, enabling
access via \\[comint-previous-input] and \\[comint-next-input].\n
The list containing all of these commands is `shx-kept-commands'.
That list can be added to using `shx-cmd-keep'."
  (if (string-empty-p regexp)
      (shx-insert 'error "kept <regexp>\n")
    (shx--restore-kept-commands regexp t)
    (shx--hint "M-x customize-variable shx-kept-commands <RET> edits this list")))
(defalias 'shx-cmd-k #'shx-cmd-kept)

(defun shx-cmd-man (topic)
  "Launch an Emacs `man' window for TOPIC.
See `Man-notify-method' for what happens when the page is ready."
  (if (equal topic "")
      (shx-insert 'error "man <topic>\n")
    (man topic)))

(defun shx-cmd-name (name)
  "(SAFE) Rename the current buffer to NAME."
  (rename-buffer (generate-new-buffer-name name)))

(defun shx-cmd-oedit (file)
  "(SAFE) open FILE in other window.
\nExamples:\n
  :oedit directory/to/file
  :oedit /username@server:~/directory/to/file"
  (setq file (car (shx-tokenize file)))
  (if file
      (find-file-other-window (expand-file-name file))
    (find-file-other-window "")))

(defun shx-cmd-pwd (_args)
  "(SAFE) Show what Emacs thinks the default directory is.
\nNote if you're at a shell prompt, you can probably use
\\[shell-resync-dirs] to reset Emacs' pwd to the shell's pwd."
  (shx-insert default-directory "\n"))

(defun shx-cmd-ssh (host)
  "Open a shell on HOST using tramp.
\nThis way you benefit from the remote host's completions, and
commands like :pwd and :edit will work correctly.
\nExample:\n
  :ssh hostname:port"
  (if (equal host "")
      (shx-insert 'error "ssh host\n")
    (let* ((host (replace-regexp-in-string ":" "#" host))
           (default-directory (concat "/" host ":~")))
      (shx))))

(defun shx-cmd-sedit (file)
  "Open local FILE using sudo (i.e. as super-user).
\nExample:\n
  :sedit /etc/passwd"
  (shx-cmd-edit (concat "/sudo::" (expand-file-name file))))


;;; graphical user commands

(defun shx-cmd-plotbar (filename)
  "(SAFE) Show barplot of FILENAME.
\nFor example, \":plotbar file.dat\" where file.dat contains:\n
  \"Topic 1\" YHEIGHT1
  \"Topic 2\" YHEIGHT2
  \"Topic 3\" YHEIGHT3"
  (shx-insert-plot (car (shx-tokenize filename))
                   (concat "set boxwidth 1.5 relative;"
                           "set style data histograms;"
                           "set xtic rotate by -40 scale 0 font \",10\";"
                           "set yrange [0:];"
                           "set style fill solid 1.0 border -1;"
                           "plot")
                   "u 2:xticlabels(1) notitle"))

(defun shx-cmd-plotmatrix (filename)
  "(SAFE) Show heatmap of FILENAME.
\nFor example, \":plotmatrix file.dat\" where file.dat contains:\n
  1.5   2    3\n  4     5    6\n  7     8    9.5"
  (shx-insert-plot (car (shx-tokenize filename))
                   (concat "set view map; unset xtics; unset ytics;"
                           "unset title; set colorbox; set palette defined"
                           "(0 \"#ffffff\", 1 \"#d5e585\", 2 \"#8cc555\","
                           "3 \"#55a550\", 4 \"#1e5500\");"
                           "plot")
                   "u 1:(-$2):3 matrix w image notitle"))

(defun shx-cmd-plotline (filename)
  "(SAFE) Show line plot of FILENAME.
\nFor example, \":plotscatter file.dat\", where file.dat contains:
  1 2\n  2 4\n  4 8\n
Or just a single column:
  1\n  2\n  3\n  5"
  (shx-insert-plot (car (shx-tokenize filename))
                   "plot" "w l lw 1 notitle"))

(defun shx-cmd-plot3d (filename)
  "(SAFE) Show surface plot of FILENAME.
Read about gnuplot's expectations of the data here:
http://www.gnuplotting.org/tag/pm3d/"
  (shx-insert-plot (car (shx-tokenize filename))
                   "unset tics;set view 4, 20, 1.4, 1;splot"
                   "w pm3d notitle"))

(defun shx-cmd-plotscatter (filename)
  "(SAFE) Show scatter plot of FILENAME.
\nFor example, \":plotscatter file.dat\", where file.dat contains:
  1 2\n  2 4\n  4 8\n
Or just a single column:
  1\n  2\n  3\n  5"
  (shx-insert-plot (car (shx-tokenize filename))
                   "plot" "w p ps 2 pt 7 notitle"))
(defalias 'shx-cmd-plot #'shx-cmd-plotscatter)

(defun shx-cmd-view (filename)
  "(SAFE) View image with FILENAME directly in the buffer."
  (shx-insert-image filename))


;;; loading

(defcustom shx-shell-mode-font-locks
  `(("#.*[^#^\n]*\\'"                             0 'font-lock-comment-face)
    ("~"                                          0 'font-lock-preprocessor-face)
    (,(regexp-opt '(">" "<" "&&" "|"))            0 'font-lock-keyword-face)
    (,(shx--quote-regexp "`" "\\\\")              0 'font-lock-preprocessor-face)
    (,(shx--quote-regexp "\"" "\\\\")             0 'font-lock-string-face)
    ;; disallow leading alphabet chars so we 'don't match on contractions'
    (,(concat "[^A-Za-z]\\(" (shx--quote-regexp "'") "\\)")
                                                  1 'font-lock-string-face)
    ("\\(\\<git\\>\\) .*\\'"                      1 'font-lock-constant-face)
    ("\\(\\<rm\\>\\) .*\\'"                       1 'font-lock-warning-face))
  "Some additional syntax highlighting for `shell-mode' only."
  :type '(alist :key-type regexp))

(defcustom shx-font-locks
  `((,(concat "[^[:alnum:]" shx-leader "]" shx-leader "\\(\\<"
              (regexp-opt (shx--all-commands 'without-prefix))
              "\\>\\).*\\'")                      1 'font-lock-keyword-face))
  "Some additional syntax highlighting for the shx minor mode."
  :type '(alist :key-type regexp))

;;;###autoload
(define-minor-mode shx-mode
  "Toggle shx-mode on or off.
\nThis minor mode provides extra functionality to shell-mode and
comint-mode in general.  Use `shx-global-mode' to enable
`shx-mode' in all buffers that support it.
\nProvides the following key bindings: \n\\{shx-mode-map}"
  :lighter shx-mode-lighter
  :keymap shx-mode-map
  (if shx-mode (shx--activate) (shx--deactivate)))

;;;###autoload
(define-globalized-minor-mode shx-global-mode shx-mode shx--turn-on :require 'shx)

(defun shx (&optional name)
  "Create a new shx-enhanced shell session.
NAME is the optional name of the new buffer.
See the function `shx-mode' for details."
  (interactive)
  (let ((name (or name (generate-new-buffer-name "*shx*"))))
    ;; switch-to-buffer first -- shell uses pop-to-buffer
    ;; which is unpredictable! :(
    (switch-to-buffer name)
    (shell name)
    ;; shx might already be active due to shx-global-mode:
    (unless shx-mode (shx-mode))))

(defun shx--activate ()
  "Add font-locks, tweak defaults, add hooks/advice."
  (setq-local shx-buffer (current-buffer))
  (when (derived-mode-p 'shell-mode)
    (font-lock-add-keywords nil shx-shell-mode-font-locks))
  (font-lock-add-keywords nil shx-font-locks)
  (setq-local shx--old-prompt-read-only comint-prompt-read-only)
  (setq-local comint-prompt-read-only nil)
  (when shx-disable-undo
    (setq-local shx--old-undo-disabled (eq t buffer-undo-list))
    (unless shx--old-undo-disabled (buffer-disable-undo)))
  ;; do this one with a delay because spacemacs tries to set this variable too:
  (shx--asynch-funcall (lambda () (setq comint-input-sender 'shx-filter-input)))
  (add-hook 'comint-output-filter-functions #'shx-parse-output-hook nil t)
  (unless (derived-mode-p 'comint-mode)
    (message "WARNING: shx is incompatible with `%s'" major-mode)))

(defun shx--deactivate ()
  "Remove font-locks and hooks, and restore variable defaults."
  (when (derived-mode-p 'shell-mode)
    (font-lock-remove-keywords nil shx-shell-mode-font-locks))
  (font-lock-remove-keywords nil shx-font-locks)
  (setq-local comint-prompt-read-only shx--old-prompt-read-only)
  (when shx-disable-undo (unless shx--old-undo-disabled (buffer-enable-undo)))
  (setq comint-input-sender 'comint-simple-send)
  (remove-hook 'comint-output-filter-functions #'shx-parse-output-hook t))

(defun shx--turn-on ()
  "Call the function `shx-mode' if appropriate."
  (when (derived-mode-p 'comint-mode) (shx-mode +1)))


;; advise some comint-mode functions -- but only within shx-mode

(defun shx-show-output (&rest _args)
  "Recenter window so that as much output as possible is shown.
This function only works when the shx minor mode is active."
  (when shx-mode
    ;; `recenter'ing errors when this isn't the active buffer:
    (ignore-errors (comint-show-maximum-output))))

(defun shx-flash-prompt (&rest _args)
  "Flash the text on the line with the highlight face."
  (setq-local shx-prompt-overlay (make-overlay (point) (point-at-eol)))
  (overlay-put shx-prompt-overlay 'face 'highlight)
  (sit-for 1)
  (delete-overlay shx-prompt-overlay))

(defun shx-snap-to-top (&rest _args)
  "Recenter window so the current line is at the top.
This function only works when the shx minor mode is active."
  (when shx-mode (recenter-top-bottom 0)))

(defun shx-switch-to-insert (&rest _args)
  "Switch to insert-mode (when applicable).
This function only works when the shx minor mode is active."
  (and shx-mode
       (featurep 'evil-vars)
       (not (equal evil-state 'insert))
       (featurep 'evil-states)
       (evil-insert-state)))

(when shx-comint-advise
  (advice-add #'comint-kill-input :before #'shx-switch-to-insert)
  (advice-add #'comint-send-input :after #'shx-switch-to-insert)
  (advice-add #'comint-history-isearch-backward-regexp :before #'shx-show-output)
  (advice-add #'comint-previous-input :before #'shx-show-output)
  (advice-add #'comint-next-input :before #'shx-show-output)
  (advice-add #'comint-kill-input :before #'shx-show-output)
  (advice-add #'comint-send-eof :before #'shx-show-output)
  (advice-add #'comint-previous-prompt :after #'shx-snap-to-top)
  (advice-add #'comint-previous-prompt :after #'shx-flash-prompt)
  (advice-add #'comint-next-prompt :after #'shx-snap-to-top)
  (advice-add #'comint-next-prompt :after #'shx-flash-prompt))

(provide 'shx)
;;; shx.el ends here
