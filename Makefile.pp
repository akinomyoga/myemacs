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
copyfiles+=$(MYDIR)/term/screen.el
$(MYDIR)/term/screen.el: term/screen.el | $(MYDIR)/term

dirs+=$(EMACSD)/lisp $(EMACSD)/lisp/auto-install
#%m lisp::add
copyfiles+=$(EMACSD)/%file%
compilefiles+=$(EMACSD)/%file%c
$(EMACSD)/%file%: %file% | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/%file%c: $(EMACSD)/%file%
#%end
#%x
add lisp/dropdown-list.el;
add lisp/fuzzy.el;
add lisp/popup.el;
add lisp/auto-complete.el;
add lisp/auto-complete-config.el;
add lisp/auto-install.el;
add lisp/csharp-mode.el;
add lisp/gnuplot.el;
add lisp/yasnippet.el;
add lisp/auto-install/anything.el;
add lisp/auto-install/anything-auto-install.el;
add lisp/auto-install/anything-complete.el;
add lisp/auto-install/anything-config.el;
add lisp/auto-install/anything-grep.el;
add lisp/auto-install/anything-gtags.el;
add lisp/auto-install/anything-ipa.el;
add lisp/auto-install/anything-match-plugin.el;
add lisp/auto-install/anything-menu.el;
add lisp/auto-install/anything-migemo.el;
add lisp/auto-install/anything-obsolete.el;
add lisp/auto-install/anything-show-completion.el;
add lisp/auto-install/anything-startup.el;
add lisp/auto-install/descbinds-anything.el;
add lisp/auto-install/ipa.el;
add lisp/auto-install/popwin.el;
#%end .r=add =#%x lisp::add.r|%file%|= .r=;=|=

dirs+=$(EMACSD)/lisp/ac-dict
#%m lisp::resource
copyfiles+=$(EMACSD)/%file%
$(EMACSD)/%file%: %file% | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
#%end
Makefile: lisp/ac-dict
#%$ls -1 lisp/ac-dict/*-mode | awk '{print "#%x lisp::resource.r|%file%|" $0 "|";}'

$(dirs):
	mkdir -p $@
$(copyfiles):
	cp -p $< $@

.SUFFIXES: .elc .el
.el.elc:
	emacs -batch -L . -L lisp -L lisp/auto-install -L $(EMACSD)/elpa/js2-mode-*/ -f batch-byte-compile $<

all:
install: $(copyfiles) $(compilefiles)
