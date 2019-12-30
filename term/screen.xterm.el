;;; screen.xterm.el --- terminal support code for screen.xterm-256color -*- no-byte-compile: t -*-
;; Treat a screen terminal similar to an xterm.

;; Copyright (C) 2011-2019 K. Murase.

(load "term/xterm")

;;; This is for GNU Emacs 21
(if (= 21 emacs-major-version)
    (load "term/xterm-256color"))

;; reference: http://www.xvx.ca/~awg/emacs-colors-howto.txt
;;; This is for GNU Emacs 22
(defun terminal-init-screen.xterm ()
  "Terminal initialization function for screen.xterm."
  ;; Use the xterm color initialization code.
  (if (< emacs-major-version 25)
      (xterm-register-default-colors)
    (xterm-register-default-colors xterm-standard-colors))
  (tty-set-up-initial-frame-faces))

;; screen.xterm.el ends here
