;; To run unit tests from the project root:
;;    `emacs -q --script ./test/script.el`

(add-to-list 'load-path ".")
(add-to-list 'load-path "test")

(require 'shx)
(require 'shx-test)

;; verify that shx.el compiles
(byte-compile-file "shx.el")

;; only the headless unit tests can be run in script mode
(dolist (test (all-completions "shx-test-unit" obarray 'functionp))
  (funcall (intern test)))

;; kill emacs with an error code corresponding to success
(kill-emacs (if shx-test-recent-success 0 1))
