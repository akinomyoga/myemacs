;;; rosaterm.el --- terminal support code for rosaterm
;; Copyright (C) 2011-2019 K. Murase.

(load "term/xterm")

;;; This is for GNU Emacs 21
(if (= 21 emacs-major-version)
    (load "term/xterm-256color"))

(eval-when-compile
  ;; variables defined in auto-complete-mode
  (defvar xterm-standard-colors)
  (declare-function xterm-register-default-colors "term/xterm"))

(defun terminal-init-rosaterm ()
  "Terminal initialization function for rosaterm."
  ;; Use the xterm color initialization code.

  (if (< emacs-major-version 25)
      (xterm-register-default-colors)
    (xterm-register-default-colors xterm-standard-colors))
  (tty-set-up-initial-frame-faces))

;;; rosaterm.el ends here
