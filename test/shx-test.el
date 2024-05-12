;;; shx-test.el --- Tests for shx

;; Authors: Chris Rayner (dchrisrayner @ gmail)
;; URL: https://github.com/riscy/shx-for-emacs
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.0.0

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

;; Tests for shx.  To run in a comint session with shx active use :test

;;; Code:

(require 'shx)

(defvar shx-test-recent-success t "Whether the last test run succeeded.")

(defun shx-cmd-test (_args)
  "Test shx.
Example:
:test"
  ;; Test the use of markup:
  (shx-insert "\n<test-all>\n"))

(defun shx-cmd-test-all (_args)
  "(SAFE) Call the 'shx-test-unit' and 'shx-test-integration' functions."
  (setq shx-test-recent-success t)
  (shx--asynch-funcall
   (lambda ()
     (dolist (test-function
              (append
               (all-completions "shx-test-unit" obarray 'functionp)
               (all-completions "shx-test-integration" obarray 'functionp)))
       (funcall (intern test-function))))
   (recenter -1)))

(defun shx-test-assert (comment val)
  "Describe test with COMMENT; test truth of VAL."
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (goto-char (point-at-eol))
    (setq shx-test-recent-success (and shx-test-recent-success val))
    (let ((output (format "%s %s\n" (if val "✔" "✘") comment)))
      (if (display-graphic-p)
          (shx-insert output)
        (send-string-to-terminal output)))))

(defun shx-test-warn (text)
  "Warn with TEXT."
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (goto-char (point-at-bol))
    (shx-insert 'error text "\n")))


;; tests!

(defun shx-test-unit-checkdoc ()
  "Run `checkdoc' against the shx.el file."
  (ignore-errors (kill-buffer "*Warnings*"))
  (let ((file (replace-regexp-in-string ".elc$" ".el" (symbol-file 'shx-mode))))
    (if (fboundp 'checkdoc-file) (checkdoc-file file))
    (shx-test-assert "shx.el passes checkdoc" (null (get-buffer "*Warnings*")))))

(defun shx-test-unit-byte-compile ()
  "Test byte-compilation against the shx.el file."
  (ignore-errors (kill-buffer "*Compile-Log*"))
  (let ((file (replace-regexp-in-string ".elc$" ".el" (symbol-file 'shx-mode))))
    (shx-test-assert "shx.el passes byte-compilation"
                     (byte-compile-file file))))

(defun shx-test-unit-declare-function ()
  "Test `declare-function'."
  (let ((file (replace-regexp-in-string ".elc$" ".el" (symbol-file 'shx-mode))))
    (shx-test-assert "shx-el passes check-declare-file"
                     (not (check-declare-file file)))))

(defun shx-test-unit-quote-regexp ()
  "Test pattern matching on delimited regexps like strings."
  (shx-test-assert
   "shx--quote-regexp matches escaped regexps correctly"
   (let ((pattern "`echo \\\\\\`echo\\\\\\``"))
     (string-match (shx--quote-regexp "`") pattern)
     (string= pattern (match-string 0 pattern))))
  (shx-test-assert
   "shx--quote-regexp matches unescaped regexps correctly"
   (let ((pattern "'don\\'t'"))
     (string-match (shx--quote-regexp "'" "") pattern)
     (string= "'don\\'" (match-string 0 pattern)))))

(defun shx-test-unit-safe-as-markup ()
  "Test recognition of safe functions."
  (shx-test-assert "shx--safe-as-markup-p recognizes unsafe command"
                   (not (shx--safe-as-markup-p #'shx-cmd-eval)))
  (shx-test-assert "shx--safe-as-markup-p recognizes a safe command"
                   (shx--safe-as-markup-p #'shx-cmd-stop))
  (shx-test-assert "shx--safe-as-markup-p handles undocumented command"
                   (not (shx--safe-as-markup-p (lambda () t)))))

(defun shx-test-unit-shell-quote-no-quotation-marks ()
  "Test `shx--shell-quote-no-quotation-marks'."
  (shx-test-assert
   "shx--shell-quote-no-quotation-marks escapes string"
   (string= (shx--shell-quote-no-quotation-marks "test\"test") "test\\\"test")))

(defun shx-test-unit-get-user-cmd ()
  "Test `shx--get-user-cmd'."
  (shx-test-assert "shx--get-user-cmd returns nil for empty string"
                   (null (shx--get-user-cmd "")))
  (shx-test-assert "shx--get-user-cmd returns command with correct prefix"
                   (eq (shx--get-user-cmd "test-al") 'shx-cmd-test-all)))

(defun shx-test-unit-shell-command ()
  "Test `shx--shell-command'."
  (shx-test-assert "shx--shell-command finds the default shell"
                   (not (string-empty-p (shx--shell-command)))))

(defun shx-test-unit-replace-from-list ()
  "Test `shx--replace-from-list'."
  (shx-test-assert "shx--replace-from-list acts sequentially"
                   (string= "a"
                            (shx--replace-from-list
                             '(("aa" "b") ("b" "a"))
                             "aa")))
  (shx-test-assert "shx--replace-from-list performs the correct replacements"
                   (string= "24"
                            (shx--replace-from-list
                             '(("1" "2") ("3" "4"))
                             "13"))))

(defun shx-test-integration-magic-insert ()
  "Test `shx-magic-insert'."
  (let ((previous-input (comint-previous-input-string 0)))
    (insert "^" previous-input "^^")
    (shx-magic-insert)
    (shx-test-assert "shx-magic-insert performs inline substitution"
                     (equal (shx--current-input) ""))
    (insert previous-input " !!")
    (shx-magic-insert)
    (shx-test-assert "shx-magic-insert performs command expansion"
                     (equal
                      (shx--current-input)
                      (concat previous-input " " previous-input)))
    (comint-kill-input)))

(defun shx-test-unit-tokenize ()
  "Test string tokenizaton."
  (shx-test-assert "shx-tokenize works with apostrophes."
                   (equal
                    '("first" "second token" "third")
                    (shx-tokenize "'first' 'second token' 'third'")))
  (shx-test-assert "shx-tokenize works with partial apostrophes."
                   (equal
                    '("first-token" "secondtoken")
                    (shx-tokenize "'first-token' secondtoken")))
  (shx-test-assert "shx-tokenize returns nil when quoting doesn't match."
                   (equal nil (shx-tokenize "first/token 'second token")))
  (shx-test-assert "shx-tokenize works with apostrophes and quotation marks."
                   (equal
                    '("first token" "second token" "3")
                    (shx-tokenize "'first token' \"second token\" 3")))
  (shx-test-assert "shx-tokenize works with escaped spaces."
                   (equal
                    '("first token" "secondtoken")
                    (shx-tokenize "first\\ token secondtoken")))
  (shx-test-assert "shx-tokenize works with escaped quotation marks."
                   (equal
                    '("\"test file\"" "'test file'")
                    (shx-tokenize "\\\"test\\ file\\\" \\\'test\\ file\\\'")))
  (shx-test-assert "shx-tokenize works with escaped characters"
                   (equal '("a" "b c.d") (shx-tokenize "a b\\ \\c\\.d")))
  (shx-test-assert "shx-tokenize works with a directory specified."
                   (equal
                    '("~/././~/.spacemacs")
                    (shx-tokenize "~/././~/.spacemacs"))))

(defun shx-test-unit-tokenize-filenames ()
  "Test filename tokenization."
  (shx-test-assert
   "shx-tokenize-filenames works with relative and absolute paths."
   (let ((comint-file-name-prefix "/docker:123:"))
     (equal
      '("test" "/docker:123:~/test" "/docker:123:/test")
      (shx-tokenize-filenames "test ~/test /test")))))

(defun shx-test-integration-point-predicates ()
  "Test some predicate functions on the point."
  (shx-test-assert "shx-point-on-input-p works at point-max."
                   (save-excursion
                     (goto-char (point-max))
                     (shx-point-on-input-p)))
  (shx-test-assert "shx-point-on-input-p works on last line and point-max."
                   (save-excursion
                     (goto-char (point-max))
                     (backward-char)
                     (not (shx-point-on-input-p))))
  (shx-test-assert "shx-point-on-input-p fails when not on last line."
                   (not (save-excursion
                          (goto-char (point-min))
                          (shx-point-on-input-p))))
  (goto-char (point-max)))

(defun shx-test-integration-propertize-prompt ()
  "Test that recent prompt gets propertized."
  (save-excursion
    (let ((shx-flash-prompt-time 0))
      (comint-previous-prompt 1)
      (forward-line 0)
      (let ((help-echo (or (get-text-property (point) 'help-echo) "")))
        (shx-test-assert "shx--propertize-prompt propertizes prompt text"
                         (string-match
                          "At [0-9][0-9]:[0-9][0-9]:[0-9][0-9]"
                          help-echo))))))

(defun shx-test-integration-input-handling ()
  "Test shx's input handling."
  (goto-char (point-max))
  (insert "test")
  (shx-test-assert "shx--current-input recognizes recent input"
                   (string= "test" (shx--current-input)))
  (comint-kill-input)
  (forward-line -2)
  (shx-send-input-or-copy-line)
  (shx-test-assert "shx--current-input copies test line"
                   (string= (substring (shx--current-input) 0 1) "✔"))
  (comint-kill-input)
  (shx-test-assert "shx--current-input recognizes blank line"
                   (string= (shx--current-input) "")))

(defun shx-test-integration-output-handling ()
  "Test `shx-insert'."
  (save-excursion
    (goto-char (point-max))
    (backward-char 1)
    (shx-test-assert "shx-insert propertizes the output"
                     (eq (field-at-pos (point)) 'output))))

(defun shx-test-unit-cmd-syntax-regexps ()
  "Test `shx-cmd-syntax' regexps."
  (string-match (concat "^" shx-leader shx-cmd-syntax) ":help ok")
  (shx-test-assert "shx-cmd-syntax recognizes command with arguments"
                   (and
                    (string= (match-string 1 ":help ok") "help")
                    (string= (match-string 2 ":help ok") "ok")))
  (string-match (concat "^" shx-leader shx-cmd-syntax) ":pwd")
  (shx-test-assert "shx-cmd-syntax recognizes alphabetical command names"
                   (string= (match-string 1 ":pwd") "pwd"))
  (string-match (concat "^" shx-leader shx-cmd-syntax) ":plot-bar")
  (shx-test-assert "shx-cmd-syntax recognizes hyphenated command names"
                   (string= (match-string 1 ":plot-bar") "plot-bar")))

(defun shx-test-unit-shx-cat ()
  "Test the `shx-cat' command."
  (let ((concatenation (shx-cat "Test" 'font-lock-string-face "test")))
    (shx-test-assert "shx-cat concatenates strings correctly"
                     (string= concatenation "Testtest"))
    (shx-test-assert "shx-cat propertizes text correctly"
                     (equal
                      (get-text-property 4 'font-lock-face concatenation)
                      'font-lock-string-face))))

(defun shx-test-unit-timers ()
  "Test functions that use Emacs' built-in timer."
  (if (shx--get-timer-list)
      (shx-test-warn "Warning: :stop all timers to run timing tests")
    (shx-test-assert "shx--get-timer-list is empty"
                     (not (shx--get-timer-list)))
    (shx--delay-input "10 sec" "stub command")
    (shx-test-assert "shx--shx-timer-list grows by 1"
                     (eq 1 (length (shx--get-timer-list))))
    (cancel-timer (car (shx--get-timer-list)))
    (shx-test-assert "shx--get-timer-list becomes empty"
                     (not (shx--get-timer-list)))))

(provide 'shx-test)
;;; shx-test.el ends here
