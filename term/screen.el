;;; screen.el --- terminal support code for screen-256color -*- no-byte-compile: t -*-
;; Treat a screen terminal similar to an xterm.

;; Copyright (C) 2011-2012 K. Murase.

(load "term/xterm")

;;; This is for GNU Emacs 21
(if (= 21 emacs-major-version)
    (load "term/xterm-256color"))

;; reference: http://www.xvx.ca/~awg/emacs-colors-howto.txt
;;; This is for GNU Emacs 22
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Use the xterm color initialization code.
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces)
  (cond
   ((string= (getenv "MWG_LOGINTERM") "cygwin")
    (load "term/cygwin")
    (cygwin-register-keymap))
   ((string= (getenv "MWG_LOGINTERM") "rosaterm")
    (load "term/rosaterm")
    (rosaterm-register-keymap))
   (t
    (load "term/rosaterm")
    (rosaterm-register-keymap))
   ))

;; screen.el ends here
