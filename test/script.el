;; Script to run unit tests at the command line:
;;    `emacs -q --script ./path/to/script.el`

(add-to-list 'load-path ".")
(add-to-list 'load-path "test")

(require 'shx)
(require 'shx-test)

;; only the headless unit tests can be run in script mode
(dolist (test (all-completions "shx-test-unit" obarray 'functionp))
  (funcall (intern test)))

;; kill emacs with an error code corresponding to success
(kill-emacs (if shx-test-recent-success 0 1))
