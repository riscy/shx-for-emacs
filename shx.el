;;; shx.el --- Extras for the comint-mode shell -*- lexical-binding: t -*-

;; Authors: Chris Rayner (dchrisrayner@gmail.com)
;; Created: May 23 2011
;; Keywords: processes, tools, comint, shell, repl
;; URL: https://github.com/riscy/shx-for-emacs
;; Package-Requires: ((emacs "24.4"))
;; Version: 1.1.2

;; This file is free software; you can redistribute or modify it under the terms
;; of the GNU General Public License <https://www.gnu.org/licenses/>, version 3
;; or any later version.  This software comes with with NO WARRANTY, not even
;; the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;; shx ("shell-extras") extends comint-mode: it parses markup in the output
;; stream, enabling plots and graphics to be embedded, and adds command-line
;; functions which plug into Emacs (e.g. use :e <filename> to edit a file).

;; Type M-x shx RET to create a new shell session using shx.
;; Type M-x customize-group RET shx RET to see customization options.
;; You can enable shx in every comint-mode buffer with (shx-global-mode 1).

;;; Code:

(require 'color)
(require 'comint)
(require 'files)
(require 'shell)
(require 'subr-x)

;; Compiler pacifier
(defvar evil-state)
(defvar tramp-syntax)
(declare-function evil-insert-state "ext:evil-states.el" (&optional arg) t)


;;; customization options and other variables

(defgroup shx nil
  "Extras for the (comint-mode) shell."
  :prefix "shx-"
  :group 'comint
  :link '(url-link :tag "GitHub" "https://github.com/riscy/shx-for-emacs"))

(defcustom shx-disable-undo nil
  "Whether to automatically disable undo in shx buffers."
  :type 'boolean)

(defcustom shx-path-to-convert "convert"
  "Path to ImageMagick's convert binary."
  :type 'string)

(defcustom shx-mode-lighter " shx"
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

(defcustom shx-flash-prompt-time 0.25
  "Length of time (in seconds) the prompt flashes, when so advised."
  :link '(function-link shx-flash-prompt)
  :type 'float)

(defcustom shx-show-hints t
  "Whether to echo hints when running certain commands."
  :type 'boolean)

(defcustom shx-triggers
  '(("https?://[A-Za-z0-9,./?=&;_-]+[^[:space:].\"'>)]+" . shx--parse-url))
  "Triggers of the form: (regexp . function)."
  :type '(alist :key-type regexp :value-type function))

(defcustom shx-kept-commands nil
  "Shell commands of the form (description . command)."
  :link '(function-link shx-cmd-kept)
  :link '(function-link shx-cmd-keep)
  :type '(alist :key-type string :value-type string))

(defcustom shx-max-input most-positive-fixnum
  "The largest input allowed in characters.
A good value on macOS is 1024, the size of the typeahead buffer;
or, set the terminal to canonical mode with 'stty -icanon'."
  :type 'integer)

(defcustom shx-max-output most-positive-fixnum
  "The length at which an output line is long enough to be broken.
Setting this to 1024 can lead to enormous performance gains, but
sacrifices the soundness of shx's markup and trigger matching."
  :link '(function-link shx--break-long-line-maybe)
  :type 'integer)

(defvar shx-cmd-prefix "shx-cmd-"
  "Prefix for user-command functions.")

(defvar shx-cmd-syntax "\\([^[:space:]]+\\)[[:space:]]*\\(.*[^[:space:]]?\\)"
  "Regex for recognizing shx commands in input or markup.")

(defvar shx-markup-syntax (concat "^<" shx-cmd-syntax ">\n")
  "Regex for recognizing shx commands in markup.")

(defvar shx-mode-map
  (let ((keymap (make-sparse-keymap)))
    (when shx-use-magic-insert
      (define-key keymap " " #'shx-magic-insert)
      (define-key keymap "q" #'shx-magic-insert))
    ;; different RET keybindings to support different Emacs environments
    (define-key keymap (kbd "RET") #'shx-send-input-or-open-thing)
    (define-key keymap (kbd "<return>") #'shx-send-input-or-open-thing)
    (define-key keymap (kbd "C-<return>") #'shx-send-input-or-copy-line)
    keymap)
  "Keymap for shx.")

(defvar shx-click-file (let ((keymap (make-sparse-keymap)))
                         (define-key keymap [mouse-1] #'ffap-at-mouse)
                         keymap)
  "Keymap for capturing mouse clicks on files/URLs.")

(defvar-local shx-buffer nil "Local reference to the shx buffer.")

(defvar-local shx-prompt-overlay nil "Overlay used to flash the prompt.")

(defvar-local shx-urls nil "Local record of URLs seen.")

(defvar-local shx--old-undo-disabled nil
  "Whether undo was disabled before `shx-mode' was enabled.")

(defvar-local shx--asynch-point nil)
(defvar-local shx--asynch-calling-buffer nil)


;;; input

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
    (let ((line (string-trim (buffer-substring-no-properties
                              (point-at-bol) (point-at-eol)))))
      (goto-char (point-max))
      (insert line))))

(defun shx-send-input ()
  "Send or parse the input currently written at the prompt.
In normal circumstances this input is additionally filtered by
`shx-filter-input' via `comint-mode'."
  (interactive)
  (cond ((not (comint-check-proc shx-buffer))
         ;; no process?  restart shell in a safe directory:
         (when (file-remote-p default-directory)
           (setq default-directory (getenv "HOME")))
         (shx--restart-shell))
        ((>= (length (shx--current-input)) shx-max-input)
         (message "Input line exceeds `shx-max-input'."))
        (t (shx--propertize-prompt)
           (comint-send-input nil t))))

(defun shx-filter-input (process input)
  "Before sending to PROCESS, filter the INPUT.
That means, if INPUT is a shx-command, do that command instead.
This function overrides `comint-input-sender'."
  (and shell-dirtrackp (string-prefix-p "z " input)
       (shx--asynch-funcall #'shell-resync-dirs))
  (let* ((regexp (concat "^" shx-leader shx-cmd-syntax))
         (match (string-match regexp (string-trim-left input)))
         (shx-cmd (and match (shx--get-user-cmd (match-string 1 input)))))
    (if (not shx-cmd)
        (comint-simple-send process input)
      (condition-case-unless-debug error-descriptor
          (funcall shx-cmd (substitute-env-vars (match-string 2 input)))
        (error (shx-insert 'error (error-message-string error-descriptor) "\n")))
      (with-current-buffer (process-buffer process)
        ;; advance the process mark to trick comint-mode
        (set-marker (process-mark process) (point)))
      ;; send a blank to fetch a new prompt
      (when (process-live-p process) (comint-send-string process "\n")))))

(defun shx--propertize-prompt ()
  "Add a mouseover timestamp and `default-directory' info to the last prompt."
  (let ((inhibit-read-only t)
        (inhibit-field-text-motion t))
    (add-text-properties
     (point-at-bol)
     (process-mark (get-buffer-process (current-buffer)))
     `(help-echo ,(format-time-string "At %X") shx-cwd ,default-directory))))


;;; output

(defun shx-parse-output-hook (&optional _output)
  "Hook to parse the output stream."
  (shx--parse-output-for-markup)
  (shx--break-long-line-maybe)
  (when shx-triggers (shx--parse-output-for-triggers)))

(defun shx--parse-output-for-markup ()
  "Look for markup in the latest output from the process."
  (save-excursion
    (shx--goto-last-input-or-output)
    (let ((originating-buffer shx-buffer))
      (while (shx--search-forward shx-markup-syntax)
        (let ((command (shx--get-user-cmd (match-string 1)))
              (args    (match-string 2)))
          (cond ((not command) nil)
                ((not (shx--safe-as-markup-p command))
                 (add-text-properties
                  (point-at-bol) (point-at-eol)
                  `(help-echo "shx: this markup was unsafe/undefined")))
                (t (replace-match "")   ; hide the markup
                   (funcall command args)
                   (set-buffer originating-buffer))))))))

(defun shx--parse-output-for-triggers ()
  "Look for triggers in the latest output from the process (e.g. URLs)."
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
  "Go to the beginning of the latest output from the process."
  (goto-char (if (marker-position comint-last-output-start)
                 (max comint-last-output-start comint-last-input-end)
               comint-last-input-end))
  (forward-line 0))

(defun shx--search-forward (pattern)
  "Search forward from the current point for PATTERN."
  (when (< (point-at-eol) (point-max))
    (re-search-forward pattern nil t)))

(defun shx--break-long-line-maybe ()
  "Break the current line if it's longer than `shx-max-output'."
  (when (> (current-column) shx-max-output)
    (or (re-search-backward "\\s-" (- (point) shx-max-output) t) (backward-char))
    (insert-char ?\n)
    (goto-char (point-max))))


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

(defun shx--all-commands (&optional without-prefix)
  "Return a list of all shx commands.
With non-nil WITHOUT-PREFIX, strip `shx-cmd-prefix' from each."
  (mapcar (lambda (cmd)
            (if without-prefix (string-remove-prefix shx-cmd-prefix cmd) cmd))
          (all-completions shx-cmd-prefix obarray #'functionp)))

(defun shx-point-on-input-p ()
  "Check if point is on the input region."
  (or (eq (point) (point-max))
      (let ((process (get-buffer-process (current-buffer))))
        (and process (>= (point-marker) (process-mark process))))))

(defun shx-tokenize (str)
  "Turn STR into a list of tokens, or nil if parsing fails.
This is robust to various styles of quoting and escaping."
  (setq str (shx--replace-from-list
             ;; protect escaped single/double quotes and spaces:
             '(("\\\\'" "") ("\\\\ " "") ("\\\\\"" "")
               ("'" "\"")               ; prefer double quoting
               ("\\\\\\(.\\)" "\\1"))   ; remove escape chars
             str))
  (mapcar (lambda (token)
            (shx--replace-from-list '(("" "'") ("" " ") ("" "\"")) token))
          (ignore-errors (split-string-and-unquote str))))

(defun shx--replace-from-list (patterns str)
  "Replace multiple PATTERNS in STR -- in the supplied order."
  (dolist (pattern patterns nil)
    (setq str (replace-regexp-in-string (car pattern) (cadr pattern) str)))
  str)

(defun shx-tokenize-filenames (str)
  "Turn STR into a list of filenames, or nil if parsing fails.
If any path is absolute, prepend `comint-file-name-prefix' to it."
  (mapcar (lambda (filename)
            (cond ((not (file-name-absolute-p filename)) filename)
                  (t (concat comint-file-name-prefix filename))))
          (shx-tokenize str)))

(defun shx--hint (text)
  "Show a hint containing TEXT."
  (when shx-show-hints (message "Hint: %s" text)))

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
  (let ((timer-list-1 (mapcar
                       (lambda (timer) (when (shx--timer-by-shx-p timer) timer))
                       timer-list)))
    ;; sort the timers for consistency
    (sort (remove nil timer-list-1)
          (lambda (first-timer second-timer)
            (string< (format "%s" (aref first-timer 5))
                     (format "%s" (aref second-timer 5)))))))

(defun shx--timer-by-shx-p (timer)
  "Return t if TIMER was created by shx."
  (string-prefix-p "(lambda nil (shx--auto" (format "%s" (aref timer 5))))

(defun shx--get-user-cmd (cmd-prefix)
  "Return user command prefixed by CMD-PREFIX, or nil."
  (let* ((prefix (format "%s%s" shx-cmd-prefix (downcase cmd-prefix)))
         (completion (try-completion prefix obarray #'functionp)))
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

(defun shx--restart-shell ()
  "Guess the shell command and use `comint-exec' to restart."
  (let ((cmd (shx--validate-shell-file-name)))
    (shx-insert 'font-lock-doc-face cmd " at " default-directory "\n")
    ;; manually align comint-file-name-prefix with the default-directory:
    (setq-local comint-file-name-prefix (or (file-remote-p default-directory) ""))
    (comint-exec (current-buffer) (buffer-name) cmd nil nil)))

(defun shx--validate-shell-file-name ()
  "Guess which shell command to run, even if on a remote host or container."
  (let ((remote-id (or (file-remote-p default-directory) ""))
        ;; guess which shell command to run per `shell' convention:
        (cmd (or explicit-shell-file-name (getenv "ESHELL") shell-file-name)))
    (cond ((file-exists-p (concat remote-id cmd)) cmd)
          (t (if (file-exists-p (concat remote-id "/bin/sh"))
                 "/bin/sh"  ; /bin/sh _usually_ exists...
               (file-remote-p (read-file-name "Shell: ") 'localname))))))

(defun shx--match-last-line (regexp)
  "Return a form to find REGEXP on the last line of the buffer."
  `(lambda (bound)
     (let ((inhibit-field-text-motion t))
       (when (eq (point-max) (point-at-eol))
         (re-search-forward ,regexp bound t)))))

(defun shx--quote-regexp (delimiter &optional escape max-length)
  "Regexp matching strings delimited by DELIMITER.
ESCAPE is the string that can be used to escape the delimiter
\(defaults to backslash; ignored when set to the empty string).
MAX-LENGTH is the length of the longest match (default 300)."
  (setq escape (or escape "\\\\"))
  (concat delimiter
          "\\("
          (unless (string= "" escape)
            (concat escape escape "\\|"      ; two escapes OR
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

(defun shx--reveal-kept-commands (&optional regexp insert-kept-command)
  "Add commands from `shx-kept-commands' into `comint-input-ring'.
REGEXP filters which commands to add.  If INSERT-KEPT-COMMAND is
not nil, then insert the command into the current buffer."
  (dolist (command shx-kept-commands nil)
    (when (string-match (or regexp ".") (concat (car command) (cdr command)))
      (when insert-kept-command (ring-insert comint-input-ring (cdr command)))
      (shx-insert 'font-lock-constant-face (car command) ": "
                  'font-lock-string-face command (cdr command) "\n"))))


;;; sending/inserting

(defun shx-magic-insert ()
  "Insert the key pressed or dynamically change the input.
`comint-magic-space' completes substitutions like '!!', '!*', or
'^pattern^replacement', and if the prompt is a colon, SPC and q
are sent straight through to the process to handle paging."
  (interactive)
  (let ((on-input (shx-point-on-input-p)))
    (if (and on-input
             (string-match "^\\s-*$" (shx--current-input))
             (string-match ":$" (shx--current-prompt)))
        (progn
          (shx--hint (format "sending '%s'" (this-command-keys)))
          (process-send-string nil (this-command-keys)))
      (unless on-input (goto-char (point-max)))
      (if shx-use-magic-insert
          (comint-magic-space 1)
        (self-insert-command 1)))))

(defun shx-cat (&rest args)
  "Like `concat' but ARGS can be strings or face names."
  (let ((string "")
        (face nil))
    (dolist (arg args nil)
      (cond ((stringp arg)
             (setq string (concat string (propertize arg 'font-lock-face face
                                                     'rear-nonsticky t))))
            ((facep arg)
             (setq face arg))))
    string))

(defun shx-insert (&rest args)
  "Insert ARGS as an output field, combined using `shx-cat'."
  (insert (propertize (apply #'shx-cat args) 'field 'output)))

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
      (shx--insert-timer (1+ timer-number) (nth timer-number sorted-timer-list))
      (shx-insert "\n"))
    (shx-insert "Active timers: " 'font-lock-constant-face
                (format "%d\n" (length sorted-timer-list)))))

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
  (let* ((img-name (make-temp-file "tmp" nil ".png"))
         (status (call-process
                  shx-path-to-gnuplot nil t nil "-e"
                  (concat
                   "set term png transparent truecolor;"
                   "set border lw 3 lc rgb \""
                   (color-lighten-name (face-attribute 'default :foreground) 5)
                   "\"; set out \"" img-name "\";"
                   plot-command " \"" (shell-quote-argument filename) "\" "
                   line-style))))
    (when (zerop status) (shx-insert-image img-name))))

(defun shx--insert-timer (timer-number timer)
  "Insert a line of the form '<TIMER-NUMBER> <TIMER>'."
  (shx-insert
   'font-lock-constant-face (format "%d. " timer-number)
   'font-lock-string-face (format "%s" (shx--format-timer-string timer))
   (when (aref timer 4) (format "\s(pulse: %d)" (aref timer 4)))))

(defun shx--format-timer-string (timer)
  "Create a human-readable string out of TIMER."
  (let* ((str (format "%s" (aref timer 5)))
         (output (string-remove-prefix "(lambda nil (shx--auto "
                                       (string-remove-suffix "))" str))))
    (concat "[" output "]")))


;;; asynch functions

(defun shx--asynch-funcall (function &optional args delay)
  "Run FUNCTION with ARGS in the buffer after a short DELAY."
  (run-at-time (or delay 0.2) nil
               `(lambda ()
                  (with-current-buffer ,shx-buffer ,(cons function args)))))

(defun shx--asynch-run (command)
  "Run shell COMMAND asynchronously; bring the results over when done."
  (if (get-buffer-process " *shx-asynch*")
      (shx-insert 'error "shx asynch was busy" 'default "\n")
    (let* ((shx-buffer_ shx-buffer)
           (output-buffer (get-buffer-create " *shx-asynch*")))
      (setq-local shx--asynch-point (point))
      (shx-insert 'font-lock-doc-face "Fetching..." 'default "\n")
      (save-window-excursion (async-shell-command command output-buffer))
      (set-buffer output-buffer)
      (setq-local shx--asynch-calling-buffer shx-buffer_)
      (let ((process (get-buffer-process output-buffer)))
        (set-process-sentinel process #'shx--asynch-sentinel)))))

(defun shx--asynch-sentinel (process _signal)
  "Sentinel for when PROCESS terminates."
  (when (memq (process-status process) '(exit signal))
    (set-buffer (process-buffer process))
    (let* ((out (buffer-substring (point-min) (point-max))))
      (set-buffer shx--asynch-calling-buffer)
      (when (>= shx--asynch-point (point-max))
        (setq shx--asynch-point 0))
      (save-excursion
        (goto-char shx--asynch-point)
        (let ((inhibit-read-only t))
          (shx-insert 'font-lock-doc-face
                      (if (string= "" out) "No output\n" out))
          ;; FIXME: this is modifying the clipboard, use delete-region
          ;; (point-at-bol) (point-at-eol)?
          (unless (= 0 shx--asynch-point) (kill-line 1)))))))


(defun shx--delay-input (delay input &optional buffer repeat-interval)
  "After DELAY, process INPUT in the BUFFER.
If BUFFER is nil, process in the current buffer.  Optional
REPEAT-INTERVAL specifies delays between repetitions."
  (let* ((process (get-buffer-process (buffer-name buffer)))
         (funcall `(lambda () ,(cons #'shx--auto (list process input)))))
    (run-at-time delay repeat-interval funcall)))

(defun shx--auto (process command)
  "Send PROCESS a COMMAND.
\(Makes the `shx-insert-timer-list' listing easier to parse.)"
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
ARGS are a string of the form '<delay in seconds> <command>'.
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
  (shx-insert-timer-list)
  (shx-insert "Asynch process " 'font-lock-constant-face
              (if (get-buffer-process " *shx-asynch*") "yes" "no") "\n"))


;;; general user commands

(defun shx-cmd-alert (string)
  "(SAFE) Show the `shx-buffer' in the other window with STRING."
  (message "From %s at %s: '%s'" shx-buffer (format-time-string "%X") string)
  (display-buffer shx-buffer))

(defun shx-cmd-clear (_args)
  "(SAFE) Clear the buffer."
  ;; this is `comint-clear-buffer' from Emacs >= 25 :
  (let ((comint-buffer-maximum-size 0)) (comint-truncate-buffer)))

(defun shx-cmd-date (_args)
  "(SAFE) Show the date."
  (shx-insert (current-time-string) "\n"))

(defun shx-cmd-diff (files)
  "(SAFE) Launch an Emacs `ediff' between FILES.
\nExample:\n
  :diff file1.txt \"file 2.csv\""
  (setq files (shx-tokenize-filenames files))
  (if (/= (length files) 2)
      (shx-insert 'error "diff <file1> <file2>" "\n")
    (shx-insert "Diffing " 'font-lock-doc-face (car files) 'default
                " and " 'font-lock-doc-face (cadr files) "\n")
    (shx--asynch-funcall #'ediff (mapcar #'expand-file-name files))))

(defun shx-cmd-edit (file)
  "(SAFE) Open FILE in the current window.
\nExamples:\n
  :e directory/to/file
\nOr edit a remote file using `tramp':\n
  :e /ssh:user@server#port:directory/to/file
  :e /docker:02fbc948e009:/directory/to/file"
  (setq file (car (shx-tokenize-filenames file)))
  (if (or (string= "" file) (not file))
      (shx-insert 'error "Couldn't parse filename" "\n")
    (shx-insert "Editing " 'font-lock-doc-face file "\n")
    (shx--asynch-funcall #'find-file (list (expand-file-name file) t))))
(defalias 'shx-cmd-e #'shx-cmd-edit)

(defun shx-cmd-eval (sexp)
  "Evaluate the elisp SEXP.
\nExamples:\n
  :eval (format \"%d\" (+ 1 2))
  :eval (* 2 (+ 3 5))"
  (let ((originating-buffer (current-buffer))
        (output (prin1-to-string (eval (car (read-from-string sexp))))))
    (with-current-buffer originating-buffer
      (shx-insert 'font-lock-constant-face "=> " output "\n"))))

(defun shx-cmd-find (file)
  "Run fuzzy find for FILE.
Depending on the contents of the current directory, this command
may take a while and unfortunately blocks Emacs in the meantime.
\nExamples:\n
  :f prefix
  :f *suffix"
  (if (equal file "")
      (shx-insert 'error "find <prefix>" "\n")
    (shx--asynch-run
     (format "find %s -iname '%s*'"
             (string-remove-suffix "/" default-directory)
             (mapconcat #'char-to-string (string-to-list file) "*")))))

(defun shx-cmd-pipe (command)
  "Pipe the output of COMMAND to a compilation buffer.
\nExamples:\n
  :pipe make
  :pipe git repack -a -d --depth=250 --window=250"
  (if (equal command "")
      (shx-insert 'error "pipe <command>" "\n")
    (switch-to-buffer-other-window "*shx-pipe*")
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
        (shx-insert 'error "Description is required" "\n")
      (add-to-list 'shx-kept-commands `(,desc . ,command))
      (customize-save-variable 'shx-kept-commands shx-kept-commands)
      (shx-insert "Keeping as " 'font-lock-doc-face desc "\n")
      (shx--hint "type ':kept' or ':k' to see all kept commands"))))

(defun shx-cmd-kept (regexp)
  "(SAFE) List the \"kept\" commands that match REGEXP.
Each matching command is appended to the input history, enabling
access via \\[comint-previous-input] and \\[comint-next-input].\n
The list containing all of these commands is `shx-kept-commands'.
That list can be added to using `shx-cmd-keep'."
  (cond ((string-empty-p regexp)
         (shx--reveal-kept-commands ".*" nil)
         (shx--hint "M-x customize-variable shx-kept-commands edits this list"))
        (t (shx--reveal-kept-commands regexp t)
           (shx--hint "Commands have been appended to session history"))))
(defalias 'shx-cmd-k #'shx-cmd-kept)

(defun shx-cmd-man (topic)
  "Launch an Emacs `man' window for TOPIC.
See `Man-notify-method' for what happens when the page is ready."
  (man topic))

(defun shx-cmd-name (name)
  "(SAFE) Rename the current buffer to NAME."
  (rename-buffer (generate-new-buffer-name name)))

(defun shx-cmd-oedit (file)
  "(SAFE) open FILE in other window.
\nExamples:\n
  :oedit directory/to/file
  :oedit /username@server:~/directory/to/file"
  (setq file (car (shx-tokenize-filenames file)))
  (if (or (string= "" file) (not file))
      (shx-insert 'error "Couldn't parse filename" "\n")
    (shx-insert "Editing " 'font-lock-doc-face file "\n")
    (find-file-other-window (expand-file-name file))))

(defun shx-cmd-pwd (_args)
  "(SAFE) Show what Emacs thinks the default directory is.
\nNote if you're at a shell prompt, you can probably use
\\[shell-resync-dirs] to reset Emacs' pwd to the shell's pwd."
  (shx-insert default-directory "\n"))

(defun shx-cmd-ssh (host)
  "Open a shell on HOST using tramp.
\nThis way you benefit from the remote host's completions, and
commands like :pwd and :edit will work correctly.  Use :ssh on
its own to point the process back at the local filesystem.
\nExample:\n
  :ssh username@hostname:port
  :ssh"
  (let ((host (substring-no-properties
               (replace-regexp-in-string ":" "#" host))))
    (setq default-directory
          (cond ((string= "" host) (getenv "HOME"))
                ((eq tramp-syntax 'default) (concat "/ssh:" host ":~"))
                (t (concat "/" host "~:"))))
    (shx--restart-shell)))

(defun shx-cmd-docker (container-id)
  "Open a shell Docker process with CONTAINER-ID."
  (let ((host (substring-no-properties
               (replace-regexp-in-string ":" "#" container-id))))
    (setq default-directory
          (cond ((string= "" host) (getenv "HOME"))
                ((eq tramp-syntax 'default) (format "/ssh:%s:~" host))
                (t (concat "/" host "~:"))))
    (shx--restart-shell)))

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
  (shx-insert-plot (car (shx-tokenize-filenames filename))
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
  (shx-insert-plot (car (shx-tokenize-filenames filename))
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
  (shx-insert-plot (car (shx-tokenize-filenames filename))
                   "plot" "w l lw 1 notitle"))

(defun shx-cmd-plot3d (filename)
  "(SAFE) Show surface plot of FILENAME.
Read about gnuplot's expectations of the data here:
http://www.gnuplotting.org/tag/pm3d/"
  (shx-insert-plot (car (shx-tokenize-filenames filename))
                   "unset tics;set view 4, 20, 1.4, 1;splot"
                   "w pm3d notitle"))

(defun shx-cmd-plotscatter (filename)
  "(SAFE) Show scatter plot of FILENAME.
\nFor example, \":plotscatter file.dat\", where file.dat contains:
  1 2\n  2 4\n  4 8\n
Or just a single column:
  1\n  2\n  3\n  5"
  (shx-insert-plot (car (shx-tokenize-filenames filename))
                   "plot" "w p ps 2 pt 7 notitle"))
(defalias 'shx-cmd-plot #'shx-cmd-plotscatter)

(defun shx-cmd-view (filename)
  "(SAFE) View image with FILENAME directly in the buffer."
  (shx-insert-image (car (shx-tokenize-filenames filename))))
(defalias 'shx-cmd-imgcat #'shx-cmd-view)


;;; loading

(defcustom shx-shell-mode-font-locks
  `((,(shx--match-last-line (shx--quote-regexp "`"))  0 'font-lock-builtin-face)
    (,(shx--match-last-line (shx--quote-regexp "\"")) 0 'font-lock-string-face)
    (,(shx--match-last-line (shx--quote-regexp "'"))  0 'font-lock-string-face)
    (,(shx--match-last-line "#.*[^#^\n]*$")           0 'font-lock-comment-face)
    (,(shx--match-last-line
       (regexp-opt '("~" ">" "<" "&" "|" ";")))       0 'font-lock-keyword-face)
    ("\\(\\<git\\>\\) .*\\'"                          1 'font-lock-constant-face)
    ("\\(\\<ssh\\>\\) .*\\'"                          1 'font-lock-constant-face)
    ("\\(\\<make\\>\\) .*\\'"                         1 'font-lock-constant-face)
    ("\\(\\<gzip\\>\\) .*\\'"                         1 'font-lock-constant-face)
    ("\\(\\<ls\\>\\) .*\\'"                           1 'font-lock-constant-face)
    ("\\(\\<mv\\>\\) .*\\'"                           1 'font-lock-warning-face)
    ("\\(\\<rmdir\\>\\) .*\\'"                        1 'font-lock-warning-face)
    ("\\(\\<rm\\>\\) .*\\'"                           1 'font-lock-warning-face))
  "Some additional syntax highlighting for `shell-mode' only."
  :type '(alist :key-type (choice regexp function)))

(defcustom shx-font-locks
  `((,(concat "[^[:alnum:]" shx-leader "]" shx-leader "\\(\\<"
              (regexp-opt (shx--all-commands 'without-prefix))
              "\\>\\).*\\'")                          1 'font-lock-keyword-face))
  "Syntax highlighting for the shx minor mode (e.g. of builtin commands)."
  :type '(alist :key-type (choice regexp function)))

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

(defun shx--activate ()
  "Add font-locks, tweak defaults, add hooks/advice."
  (if (not (derived-mode-p 'comint-mode))
      (error "WARNING: shx is incompatible with `%s'" major-mode)
    (when (derived-mode-p 'shell-mode)
      (font-lock-add-keywords nil shx-shell-mode-font-locks))
    (font-lock-add-keywords nil shx-font-locks)
    (setq-local shx-buffer (current-buffer))
    (setq-local shx--old-undo-disabled (eq t buffer-undo-list))
    (when shx-disable-undo (buffer-disable-undo))
    ;; do this one with a delay because spacemacs tries to set this variable too:
    (shx--asynch-funcall (lambda () (setq comint-input-sender 'shx-filter-input)))
    (make-local-variable 'comint-output-filter-functions)
    (make-local-variable 'comint-input-filter-functions)
    (add-hook 'comint-output-filter-functions #'shx-parse-output-hook nil t)
    (shx--advise)))

(defun shx--deactivate ()
  "Remove font-locks and hooks, and restore variable defaults."
  (when (derived-mode-p 'shell-mode)
    (font-lock-remove-keywords nil shx-shell-mode-font-locks))
  (font-lock-remove-keywords nil shx-font-locks)
  (unless shx--old-undo-disabled (buffer-enable-undo))
  (setq comint-input-sender 'comint-simple-send)
  (remove-hook 'comint-output-filter-functions #'shx-parse-output-hook t))

;;;###autoload
(define-globalized-minor-mode shx-global-mode shx-mode shx--global-on :require 'shx)

(defun shx--global-on ()
  "Call the function `shx-mode' if appropriate for the buffer."
  (when (derived-mode-p 'comint-mode) (shx-mode +1)))

(defun shx (&optional name directory)
  "Create a new shx-enhanced shell session.
The new buffer is called NAME and uses DIRECTORY as its `default-directory'.
See the function `shx-mode' for details."
  (interactive)
  (let ((name (or name (generate-new-buffer-name "*shx*")))
        (default-directory (or directory default-directory)))
    ;; `switch-to-buffer' first (`shell' uses the unpredictable `pop-to-buffer')
    (switch-to-buffer name)
    (shx--validate-shell-file-name)
    (shell name)
    ;; shx might already be active due to shx-global-mode:
    (unless shx-mode (shx-mode))))


;;; advice to change the behavior of some functions within `shx-mode'

(defun shx-show-output (&rest _args)
  "Recenter window so that as much output as possible is shown.
This function only works when the shx minor mode is active."
  (and shx-mode shx-comint-advise
       ;; `recenter'ing errors when this isn't the active buffer:
       (ignore-errors (comint-show-maximum-output))))

(defun shx-flash-prompt (&rest _args)
  "Flash the text on the line with the highlight face."
  (when (and shx-comint-advise (> shx-flash-prompt-time 0))
    (setq-local shx-prompt-overlay (make-overlay (point) (point-at-eol)))
    (overlay-put shx-prompt-overlay 'face 'highlight)
    (sit-for shx-flash-prompt-time)
    (delete-overlay shx-prompt-overlay)))

(defun shx-snap-to-top (&rest _args)
  "Recenter window so the current line is at the top.
This function only works when the shx minor mode is active."
  (and shx-mode shx-comint-advise (recenter-top-bottom 0)))

(defun shx-switch-to-insert (&rest _args)
  "Switch to insert-mode (when applicable).
This function only works when the shx minor mode is active."
  (and shx-mode
       shx-comint-advise
       (featurep 'evil-vars)
       (not (equal evil-state 'insert))
       (featurep 'evil-states)
       (evil-insert-state)))

(defun shx--with-shx-cwd (func &rest args)
  "Call FUNC with ARGS using the `shx-cwd' property as `default-directory'."
  (if (not shx-mode)
      (apply func args)
    (let* ((shx-comint-advise nil)
           (inhibit-field-text-motion t)
           (shx-cwd (save-excursion
                      (comint-previous-prompt 1)
                      (get-text-property (point-at-bol) 'shx-cwd)))
           (default-directory (or shx-cwd default-directory)))
      (apply func args))))

(defun shx--advise ()
  "Advise a number of functions with shx enhancements."
  (advice-add #'find-file-at-point :around #'shx--with-shx-cwd)
  (advice-add #'ffap-at-mouse :around #'shx--with-shx-cwd)
  (when shx-comint-advise
    (advice-add #'comint-kill-input :before #'shx-switch-to-insert)
    (advice-add #'comint-send-input :after #'shx-switch-to-insert)
    (advice-add #'comint-history-isearch-backward-regexp :before #'shx-show-output)
    (advice-add #'comint-kill-input :before #'shx-show-output)
    (advice-add #'comint-send-eof :before #'shx-show-output)
    ;; NOTE: comint-next-prompt is called by comint-previous prompt too
    (advice-add #'comint-next-prompt :after #'shx-snap-to-top)
    (advice-add #'comint-next-prompt :after #'shx-flash-prompt)))

(defun shx-unload-function ()
  "Pre-cleanup when `unload-feature' is called."
  (advice-remove #'find-file-at-point #'shx--with-shx-cwd)
  (advice-remove #'ffap-at-mouse #'shx--with-shx-cwd)
  (advice-remove #'comint-kill-input #'shx-switch-to-insert)
  (advice-remove #'comint-send-input #'shx-switch-to-insert)
  (advice-remove #'comint-history-isearch-backward-regexp #'shx-show-output)
  (advice-remove #'comint-kill-input #'shx-show-output)
  (advice-remove #'comint-send-eof #'shx-show-output)
  (advice-remove #'comint-next-prompt #'shx-snap-to-top)
  (advice-remove #'comint-next-prompt #'shx-flash-prompt)
  nil)

(provide 'shx)
;;; shx.el ends here
