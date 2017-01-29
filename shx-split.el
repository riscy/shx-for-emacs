;;; shx-split.el -- split a comint-mode buffer while scrolling

;; Authors: Chris Rayner (dchrisrayner @ gmail)
;; Created: May 23 2011
;; Keywords: comint-mode, shell-mode
;; Git: github.com/riscy/shx-for-emacs

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Automatically split the buffer into a head and a tail when you page up and
;; down in a scrolling buffer (such as shell-mode, comint-mode, ...) to help you
;; keep context when referring to earlier output.
;;
;; +--------------+
;; | -------      |
;; | -------      |
;; | -------      |
;; |    [head]    |
;; |(show history)|
;; +--------------+
;; |    [tail]    |
;; |(show context)|
;; +--------------+
;;
;; This version tested with Emacs 25.1.1
;;
;; See README.org for more details.

;;; Installation:

;; 1. Move this file to a directory in your load-path or add
;;    this to your .emacs:
;;    (add-to-list 'load-path "~/path/to/this-file/")
;; 2. Next add this line to your .emacs:
;;    (require 'shx-split)
;;
;; By default, shx-split runs automatically in comint-mode buffers.

;;; Code:

(require 'comint)
(require 'shx)

(defgroup shx-split nil
  "Extras for the (comint-mode) shell."
  :prefix "shx-split-"
  :group 'shx
  :link '(url-link
          :tag "Github"
          "https://github.com/riscy/shx-for-emacs"))

(defcustom shx-split-rows 12
  "How large the tail will be.")

(defvar-local shx-split-active nil
  "Whether the split is active.")

(defvar-local shx-split-min 30
  "The minimum window height before splitting is allowed.")

(defvar-local shx-default-scroll-on-output
  "Internal variable for remembering user scroll options.")

(defvar-local shx-default-scroll-on-input
  "Internal variable for remembering user scroll options.")

(defun shx-split-on-tail? ()
  "True if the cursor is on the tail window."
  (and (eq (current-buffer) (window-buffer (previous-window)))
       ;; check if the bottom window is approximately the right size
       (< (abs (- (window-height) shx-split-rows)) 5)))

(defun shx-split-on-head? ()
  "True if the cursor is on the head window."
  (and (eq (current-buffer) (window-buffer (next-window)))
       ;; check if the bottom window is approximately the right size
       (< (abs (- (window-height (next-window)) shx-split-rows)) 5)))

(defun shx-unsplittable? ()
  "True if the window is too small to be split."
  (or (not (shx-point-on-input?))
      (and (not (shx-scroll-find-tail))
           (< (window-height) shx-split-min))))

(defun shx-scroll-begin ()
  "Create the head/tail window pair."
  (interactive)
  (goto-char (point-max))
  (save-excursion
    ;; open a small window below
    (split-window-vertically (- (window-height) shx-split-rows)))
  (setq-local shx-split-active t)
  ;; remember previous comint settings regarding the scrolling
  (setq-local shx-default-scroll-on-output comint-scroll-to-bottom-on-output)
  (setq-local shx-default-scroll-on-input comint-scroll-to-bottom-on-input)
  ;; only auto-scroll the window the user's cursor is in
  (setq-local comint-scroll-to-bottom-on-output "this")
  (setq-local comint-scroll-to-bottom-on-input nil))

(defun shx-scroll-end ()
  "If the window is split, remove the split.
See `shx-scroll-up' and `shx-scroll-down'."
  (interactive)
  (when (not (shx-scroll-find-tail))
    (goto-char (point-max))
    (recenter -1))
  (when (shx-scroll-find-tail)
    (goto-char (point-max))
    (setq-local comint-scroll-to-bottom-on-output shx-default-scroll-on-output)
    (setq-local comint-scroll-to-bottom-on-input shx-default-scroll-on-input)
    (windmove-up) ;; go to the head?
    (delete-window))
  (setq-local shx-split-active nil)
  ;; realign
  (goto-char (point-max))
  (recenter -1))

(defun shx-scroll-find-tail ()
  "Find the tail window.
Put the cursor on the tail at the end of buffer, or return nil if
the tail is not visible and/or the matching buffer is not above."
  (cond ((shx-split-on-tail?) t)
        ((shx-split-on-head?) (select-window (next-window)))
        (t nil)))

(defun shx-scroll-up (&optional home)
  "Scroll up in the buffer.
If the window is not split, try to split it.  Then scroll the top
window up.  If HOME is non-nil, scroll all the way to the top."
  (interactive)
  (cond
   ((shx-unsplittable?)
    (if home
        (goto-char (point-min))
      (let ((line-move-visual t))
        (ignore-errors (line-move (- shx-split-rows))))
      (recenter -1)))
   (t
    (if (shx-scroll-find-tail)
        (select-window (previous-window))
      (shx-scroll-begin))
    (if home
        (goto-char (point-min))
      (let ((line-move-visual t))
        (ignore-errors (line-move (- shx-split-rows))))
      (recenter -1)
      (move-to-window-line -1))
    (select-window (next-window))
    (goto-char (point-max))
    (recenter -1))))                    ; realign tail

(defun shx-scroll-down ()
  "Scroll down in the buffer.
If the window is split, scroll the top window only.  If scrolling
down scrolls all the way down to the prompt, remove the split."
  (interactive)
  (if (not (shx-scroll-find-tail))
      (let ((line-move-visual t))
        (ignore-errors (line-move shx-split-rows)))
    (select-window (previous-window))
    (move-to-window-line -1)
    (let ((line-move-visual t))
      (ignore-errors (line-move shx-split-rows)))
    (recenter -1)
    ;; go to end of line so that on-last-line works:
    (if (save-excursion (end-of-line) (shx-point-on-input?))
        (shx-scroll-end)
      (select-window (next-window)))
    (goto-char (point-max)))
  (recenter -1))

(defun shx-scroll-home ()
  "Scroll all the way to the top."
  (interactive)
  (shx-scroll-up 'home))

(defun shx-split-mode-map (&optional parent)
  "Keymap used for `shx-split'.
PARENT keymap is optional."
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap parent)
    (when (featurep 'evil-states)
      (define-key evil-insert-state-local-map (kbd "C-u") nil)
      (define-key evil-normal-state-local-map (kbd "C-u") nil)
      (define-key evil-insert-state-local-map (kbd "C-d") nil)
      (define-key evil-normal-state-local-map (kbd "C-d") nil))
    (define-key keymap (kbd "C-u") #'shx-scroll-up)
    (define-key keymap (kbd "C-d") #'shx-scroll-down)
    (define-key keymap (kbd "<prior>") #'shx-scroll-up)
    (define-key keymap (kbd "<next>") #'shx-scroll-down)
    (define-key keymap (kbd "<home>") #'shx-scroll-home)
    (define-key keymap (kbd "M-<") #'shx-scroll-home)
    (define-key keymap (kbd "<end>") #'shx-scroll-end)
    (define-key keymap (kbd "M->") #'shx-scroll-end)
    keymap))

(defun shx-split-activate ()
  "Activate shx-split in the current buffer."
  (interactive)
  (use-local-map (shx-split-mode-map (current-local-map))))

;; Run whenever comint runs
(add-hook 'comint-mode-hook 'shx-split-activate)

(provide 'shx-split)
;;; shx-split ends here
