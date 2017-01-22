;;; shx-test -- for testing shx and shx-split.

;; Authors: Chris Rayner (dchrisrayner @ gmail)
;; Created: May 23 2011
;; Keywords: comint-mode, shell-mode
;; Git: https://github.com/riscy/shx-for-emacs

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A handful of tests for shx.  Coverage isn't great yet.
;; To run on a comint session with shx active type :test

;;; Code:

(require 'shx)
(require 'shx-split)

(defun shx-assert (comment val)
  "Describe test with COMMENT; test truth of VAL."
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (goto-char (point-at-eol))
    (if val
        (shx-insert 'font-lock-string-face "✓")
      (shx-insert 'error (format "\n  Test failed: %s\n" comment)))))

(defun shx-test-shx ()
  "Test core shx functions."
  (shx-assert "Image height is a positive integer."
              (and (integerp shx-img-height) (> shx-img-height 0)))
  (shx-assert "Filename splitting works with apostrophes."
              (equal '("first file" "second file" "third")
                     (shx--parse-filenames "'first file' 'second file' 'third'")))
  (shx-assert "Filename splitting works with mixed apostrophes."
              (equal '("first file" "secondfile")
                     (shx--parse-filenames "'first file' secondfile")))
  (shx-assert "Filename splitting works with escaped spaces."
              (equal '("first file" "secondfile")
                     (shx--parse-filenames "first\\ file secondfile")))
  (shx-assert "Filename splitting works with current directory specified."
              (equal '("~/././~/.spacemacs")
                     (shx--parse-filenames "~/././~/.spacemacs")))
  (shx-assert "Point on last line works at point-max."
              (save-excursion (goto-char (point-max))
                              (shx-point-on-input?)))
  (shx-assert "Point on last line works on last line and point-max."
              (save-excursion (goto-char (point-max)) (backward-char)
                              (not (shx-point-on-input?))))
  (shx-assert "Point on last line fails when not on last line."
              (not (save-excursion (goto-char (point-min))
                                   (shx-point-on-input?))))
  (goto-char (point-max)))

(defun shx-test-split ()
  "Test window splitting functions."
  (goto-char (point-max))
  (when (< (window-height) shx-split-min)
    (forward-line -1)
    (end-of-line)
    (shx-insert 'error "\nWarning: window too short to test shx-split"))
  (shx-assert "Create split."
              (let ((currpt (point)))
                (shx-scroll-up)
                (and shx-split-active
                     (eq currpt (point)))))
  (shx-assert "Maintain split."
              (let ((currpt (point)))
                (shx-scroll-home)
                (eq currpt (point))))
  (shx-assert "Destroy split."
              (let ((currpt (point)))
                (shx-scroll-end)
                (and (not shx-split-active)
                     (eq currpt (point)))))
  (shx-assert "Create and destroy with home/end."
              (let ((currpt (point)))
                (shx-scroll-home)
                (shx-scroll-end)
                (eq currpt (point))))
  (shx-assert "Create destroy with pgup/pgdn."
              (let ((currpt (point)))
                (shx-scroll-up) (shx-scroll-up)
                (shx-scroll-down) (shx-scroll-down) (shx-scroll-down)
                (eq currpt (point))))
  (shx-assert "Try to find nonexistent split."
              (null (shx-scroll-find-tail))))

(defun shx-cmd/test (-)
  "(SAFE) Test shx - args are ignored.
Run tests by typing :test at the console."
  (insert "Running test suite \n")
  (shx--asynch-funcall
   (lambda ()
     (shx-test-shx)
     (shx-test-split))))

(provide 'shx-test)
;;; shx-test ends here
