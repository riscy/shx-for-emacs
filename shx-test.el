;;; shx-test.el --- Tests for shx and shx-split

;; Authors: Chris Rayner (dchrisrayner @ gmail)
;; Homepage: https://github.com/riscy/shx-for-emacs

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
(require 'shx-split)

(defun shx-cmd-test (_args)
  "(SAFE) Test shx.
In particular run every function with the prefix shx-test-case.
Example:
:test"
  (insert "\n")
  (shx--asynch-funcall
   (lambda ()
     (dolist (test-function (all-completions "shx-test-case" obarray 'functionp))
       (funcall (intern test-function)))
     (message "Success!")
     (recenter -1))))

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

(defun shx-test-case-shx ()
  "Test core shx functions."
  (shx-test-assert "Image height is a positive integer."
                   (and (integerp shx-img-height) (> shx-img-height 0))))

(defun shx-test-case-magic-insert ()
  "Test magic insert."
  (insert "^:test^^")
  (shx-magic-insert)
  (shx-test-assert "Inline substitution with magic insert works."
                   (equal (shx--current-input) ""))
  (insert ":test !!")
  (shx-magic-insert)
  (shx-test-assert "Previous command expansion with magic insert works."
                   (equal (shx--current-input) ":test :test"))
  (comint-kill-input))

(defun shx-test-case-filename-parsing ()
  "Test filename parsing."
  (shx-test-assert "Filename splitting works with apostrophes."
                   (equal '("first file" "second file" "third")
                          (shx--parse-filenames "'first file' 'second file' 'third'")))
  (shx-test-assert "Filename splitting works with mixed apostrophes."
                   (equal '("first file" "secondfile")
                          (shx--parse-filenames "'first file' secondfile")))
  (shx-test-assert "Filename splitting works with escaped spaces."
                   (equal '("first file" "secondfile")
                          (shx--parse-filenames "first\\ file secondfile")))
  (shx-test-assert "Filename splitting works with current directory specified."
                   (equal '("~/././~/.spacemacs")
                          (shx--parse-filenames "~/././~/.spacemacs"))))

(defun shx-test-case-point-predicates ()
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

(defun shx-test-case-input-handling ()
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

(defun shx-test-case-cmd-syntax-regexps ()
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

(defun shx-test-case-split ()
  "Test window splitting functions."
  (goto-char (point-max))
  (if (< (window-height) shx-split-min)
      (shx-test-warn "Warning: window too short to test shx-split")
    (shx-test-assert "Create split."
                     (let ((currpt (point)))
                       (shx-split-scroll-up)
                       (and shx-split-active
                            (eq currpt (point)))))
    (shx-test-assert "Maintain split."
                     (let ((currpt (point)))
                       (shx-split-home)
                       (eq currpt (point))))
    (shx-test-assert "Destroy split."
                     (let ((currpt (point)))
                       (shx-split-end)
                       (and (not shx-split-active)
                            (eq currpt (point)))))
    (shx-test-assert "Create and destroy with home/end."
                     (let ((currpt (point)))
                       (shx-split-home)
                       (shx-split-end)
                       (eq currpt (point))))
    (shx-test-assert "Create destroy with pgup/pgdn."
                     (let ((currpt (point)))
                       (shx-split-scroll-up) (shx-split-scroll-up)
                       (shx-split-scroll-down) (shx-split-scroll-down) (shx-split-scroll-down)
                       (eq currpt (point))))
    (shx-test-assert "Try to find nonexistent split."
                     (null (shx-split-find-tail)))))

(defun shx-test-case-shx-cat ()
  "Test the `shx-cat' command."
  (let ((concatenation (shx-cat "Test" 'font-lock-string-face "test")))
    (shx-test-assert "Concatenates the strings."
                     (string= concatenation "Testtest"))
    (shx-test-assert "Propertizes the text."
                     (equal (get-text-property 4 'font-lock-face concatenation)
                            'font-lock-string-face))))

(defun shx-test-case-timers ()
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
