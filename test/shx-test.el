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

;; A handful of tests for shx.  Coverage isn't great yet.
;; To run on a comint session with shx active type :test

;;; Code:

(require 'shx)

(defun shx-cmd-test (_args)
  "Test shx.
Example:
:test"
  ;; Test the use of markup:
  (insert "\n<test-all>\n"))

(defun shx-cmd-test-all (_args)
  "(SAFE) Call the 'shx-test-unit' and 'shx-test-integration' functions."
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
    (if (not val)
        (shx-insert 'error (format "✘ %s\n" comment))
      (shx-insert 'font-lock-string-face "✔ " 'default comment "\n")
      t)))

(defun shx-test-warn (text)
  "Warn with TEXT."
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (goto-char (point-at-bol))
    (shx-insert 'error text "\n")))


;; tests!

(defun shx-test-unit-escape-filename ()
  "Test filename escaping."
  (shx-test-assert
   "Dangerous characters are escaped in filenames."
   (string-suffix-p "a\\ filename\\`delete\\`"
    (shx--escape-filename "a filename`delete`"))))

(defun shx-test-unit-quote-regexp ()
  "Test pattern matching on delimited regexps like strings."
  (shx-test-assert
   "Escaped regexps are matched."
   (let ((pattern "`echo \\\\\\`echo\\\\\\``"))
     (string-match (shx--quote-regexp "`" "\\\\") pattern)
     (string= pattern (match-string 0 pattern))))
  (shx-test-assert
   "Unescaped regexps are matched."
   (let ((pattern "'don\'t'"))
     (string-match (shx--quote-regexp "'") pattern)
     (string= "'don\'" (match-string 0 pattern)))))

(defun shx-test-unit-safe-as-markup ()
  "Test recognition of safe functions."
  (shx-test-assert "The eval function is not safe."
                   (not (shx--safe-as-markup-p (intern "shx-cmd-eval"))))
  (shx-test-assert "The stop function is safe."
                   (shx--safe-as-markup-p (intern "shx-cmd-stop"))))

(defun shx-test-integration-magic-insert ()
  "Test magic insert."
  (let ((previous-input (comint-previous-input-string 0)))
    (insert "^" previous-input "^^")
    (shx-magic-insert)
    (shx-test-assert "Inline substitution with magic insert works."
                     (equal (shx--current-input) ""))
    (insert previous-input " !!")
    (shx-magic-insert)
    (shx-test-assert "Previous command expansion with magic insert works."
                     (equal (shx--current-input)
                            (concat previous-input " " previous-input)))
    (comint-kill-input)))

(defun shx-test-unit-tokenize ()
  "Test string tokenizaton."
  (shx-test-assert "Tokenization works with apostrophes."
                   (equal '("first" "second token" "third")
                          (shx-tokenize "'first' 'second token' 'third'")))
  (shx-test-assert "Tokenization works with partial apostrophes."
                   (equal '("first-token" "secondtoken")
                          (shx-tokenize "'first-token' secondtoken")))
  (shx-test-assert "Tokenization returns nil when quoting doesn't match."
                   (equal nil (shx-tokenize "first/token 'second token")))
  (shx-test-assert "Tokenization works with apostrophes and quotation marks."
                   (equal '("first token" "second token" "3")
                          (shx-tokenize "'first token' \"second token\" 3")))
  (shx-test-assert "Tokenization works with escaped spaces."
                   (equal '("first token" "secondtoken")
                          (shx-tokenize "first\\ token secondtoken")))
  (shx-test-assert "Tokenization works with escaped quotation marks."
                   (equal '("\"test file\"" "'test file'")
                          (shx-tokenize "\\\"test\\ file\\\" \\\'test\\ file\\\'")))
  (shx-test-assert "Tokenization works with escaped characters"
                   (equal '("a" "b c.d") (shx-tokenize "a b\\ \\c\\.d")))
  (shx-test-assert "Tokenization works with a directory specified."
                   (equal '("~/././~/.spacemacs")
                          (shx-tokenize "~/././~/.spacemacs"))))

(defun shx-test-integration-point-predicates ()
  "Test some predicate functions on the point."
  (shx-test-assert "Point on last line works at point-max."
                   (save-excursion (goto-char (point-max))
                                   (shx-point-on-input-p)))
  (shx-test-assert "Point on last line works on last line and point-max."
                   (save-excursion (goto-char (point-max)) (backward-char)
                                   (not (shx-point-on-input-p))))
  (shx-test-assert "Point on last line fails when not on last line."
                   (not (save-excursion (goto-char (point-min))
                                        (shx-point-on-input-p))))
  (goto-char (point-max)))

(defun shx-test-integration-input-handling ()
  "Test shx's input handling."
  (goto-char (point-max))
  (insert "test")
  (shx-test-assert "Recent input is recognized."
                   (string= "test" (shx--current-input)))
  (comint-kill-input)
  (forward-line -2)
  (shx-send-input-or-copy-line)
  (shx-test-assert "Test line is copied"
                   (string= (substring (shx--current-input) 0 1) "✔"))
  (comint-kill-input)
  (shx-test-assert "Blank input is recognized."
                   (string= (shx--current-input) "")))

(defun shx-test-unit-cmd-syntax-regexps ()
  "Test shx-cmd-syntax regexps."
  (string-match (concat "^" shx-leader shx-cmd-syntax) ":help ok")
  (shx-test-assert "Recognizing a command with arguments."
                   (and (string= (match-string 1 ":help ok") "help")
                        (string= (match-string 2 ":help ok") "ok")))
  (string-match (concat "^" shx-leader shx-cmd-syntax) ":pwd")
  (shx-test-assert "Recognizing alphabetical command names."
                   (string= (match-string 1 ":pwd") "pwd"))
  (string-match (concat "^" shx-leader shx-cmd-syntax) ":plot-bar")
  (shx-test-assert "Recognizing hyphenated command names."
                   (string= (match-string 1 ":plot-bar") "plot-bar")))

(defun shx-test-unit-shx-cat ()
  "Test the `shx-cat' command."
  (let ((concatenation (shx-cat "Test" 'font-lock-string-face "test")))
    (shx-test-assert "Concatenates the strings."
                     (string= concatenation "Testtest"))
    (shx-test-assert "Propertizes the text."
                     (equal (get-text-property 4 'font-lock-face concatenation)
                            'font-lock-string-face))))

(defun shx-test-unit-timers ()
  "Test functions that use Emacs' built-in timer."
  (if (shx--get-timer-list)
      (shx-test-warn "Warning: :stop all timers to run timing tests")
    (shx-test-assert "The timer list starts empty." (not (shx--get-timer-list)))
    (shx--delay-input "10 sec" "stub command")
    (shx-test-assert "The timer list is not empty." (eq 1 (length (shx--get-timer-list))))
    (cancel-timer (car (shx--get-timer-list)))
    (shx-test-assert "The timer list is empty again." (not (shx--get-timer-list)))))

(provide 'shx-test)
;;; shx-test.el ends here
