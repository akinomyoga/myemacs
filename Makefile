# -*- makefile-gmake -*-

all:
.PHONY: all dist

all: mwg.elc mwg-c++exp.elc mwg-doxygen.elc term/cygwin.elc term/rosaterm.elc
all: mwg-js2-config.elc ttx-mode.elc
all: css-mode.elc

.SUFFIXES: .elc .el
.el.elc:
	emacs -batch -L . -L ../elpa/js2-mode-*/ -f batch-byte-compile $<
