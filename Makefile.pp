# -*- makefile-gmake -*-

all:
.PHONY: all dist install

EMACSD:=$(HOME)/.emacs.d
MYDIR:=$(EMACSD)/my

dirs+=$(MYDIR)
#%m my::add
copyfiles+=$(MYDIR)/%file%
compilefiles+=$(MYDIR)/%file%c
$(MYDIR)/%file%: %file% | $(MYDIR)
$(MYDIR)/%file%c: $(MYDIR)/%file%
#%end
#%x my::add.r|%file%|mwg.el|
#%x my::add.r|%file%|mwg-c++exp.el|
#%x my::add.r|%file%|mwg-doxygen.el|
#%x my::add.r|%file%|mwg-js2-config.el|
#%x my::add.r|%file%|ttx-mode.el|
#%x my::add.r|%file%|css-mode.el|

dirs+=$(MYDIR)/term
#%m term::add
copyfiles+=$(MYDIR)/%file%
compilefiles+=$(MYDIR)/%file%c
$(MYDIR)/%file%: %file% | $(MYDIR)/term
$(MYDIR)/%file%c: $(MYDIR)/%file%
#%end
#%x term::add.r|%file%|term/cygwin.el|
#%x term::add.r|%file%|term/rosaterm.el|

dirs+=$(EMACSD)/lisp
#%m lisp::add
copyfiles+=$(EMACSD)/%file%
compilefiles+=$(EMACSD)/%file%c
$(EMACSD)/%file%: %file% | $(EMACSD)/lisp
$(EMACSD)/%file%c: $(EMACSD)/%file%
#%end
#%x lisp::add.r|%file%|lisp/dropdown-list.el|
#%x lisp::add.r|%file%|lisp/fuzzy.el|
#%x lisp::add.r|%file%|lisp/popup.el|
#%x lisp::add.r|%file%|lisp/auto-complete.el|
#%x lisp::add.r|%file%|lisp/auto-complete-config.el|
#%x lisp::add.r|%file%|lisp/auto-install.el|
#%x lisp::add.r|%file%|lisp/csharp-mode.el|
#%x lisp::add.r|%file%|lisp/gnuplot.el|
#%x lisp::add.r|%file%|lisp/yasnippet.el|

dirs+=$(EMACSD)/lisp/ac-dict
#%m lisp::resource
copyfiles+=$(EMACSD)/%file%
$(EMACSD)/%file%: %file% | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
#%end
Makefile: lisp/ac-dict
#%x
#%$ls -1 lisp/ac-dict/*-mode
#%end .r=^|\n=&#%x lisp::resource.r|%file%|= .r=$|\n=|&=

$(dirs):
	mkdir -p $@
$(copyfiles):
	cp -p $< $@

.SUFFIXES: .elc .el
.el.elc:
	emacs -batch -L . -L lisp -L $(EMACSD)/elpa/js2-mode-*/ -f batch-byte-compile $<

all:
install: $(copyfiles) $(compilefiles)
