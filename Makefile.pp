# -*- makefile-gmake -*-

all:
.PHONY: all dist install

EMACSD:=$(HOME)/.emacs.d
MYDIR:=$(EMACSD)/my

#%#------------------------------------
#%m mylisp::copy
#%%[fdst="%file%"]
#%%[fsrc=fdst.replace("^my/","")]
#%%x
# mylisp::copy %file%
copyfiles+=$(EMACSD)/%file%
$(EMACSD)/%file%: ${fsrc} | $(EMACSD)%directory%

#%%end.i
#%end
#%#------------------------------------
#%m mylisp::elc
#%%[fdst="%file%"]
#%%[fsrc=fdst.replace("^my/","")]
#%%x
# mylisp::elc %file%
copyfiles+=$(EMACSD)/%file%
compilefiles+=$(EMACSD)/%file%c
$(EMACSD)/%file%: ${fsrc} | $(EMACSD)%directory%
$(EMACSD)/%file%c: $(EMACSD)/%file%

#%%end.i
#%end
#%#------------------------------------
#%m 1
dirs+=$(MYDIR) $(MYDIR)/term
elc my/mwg.el;
elc my/mwg-c++exp.el;
elc my/mwg-doxygen.el;
elc my/mwg-js2-config.el;
elc my/ttx-mode.el;
elc my/css-mode.el;
elc my/term/cygwin.el;
elc my/term/rosaterm.el;
copy my/term/screen.el;

dirs+=$(EMACSD)/lisp $(EMACSD)/lisp/auto-install
#elc lisp/csharp-mode.el;
elc lisp/gnuplot.el;

# elc lisp/fuzzy.el;
# elc lisp/popup.el;
# elc lisp/auto-complete.el;
# elc lisp/auto-complete-config.el;

# elc lisp/dropdown-list.el;
# elc lisp/yasnippet.el;

# elc lisp/auto-install.el;
# elc lisp/auto-install/anything.el;
# elc lisp/auto-install/anything-auto-install.el;
# elc lisp/auto-install/anything-complete.el;
# elc lisp/auto-install/anything-config.el;
# elc lisp/auto-install/anything-grep.el;
# elc lisp/auto-install/anything-gtags.el;
# elc lisp/auto-install/anything-ipa.el;
# elc lisp/auto-install/anything-match-plugin.el;
# elc lisp/auto-install/anything-menu.el;
# elc lisp/auto-install/anything-migemo.el;
# elc lisp/auto-install/anything-obsolete.el;
# elc lisp/auto-install/anything-show-completion.el;
# elc lisp/auto-install/anything-startup.el;
# elc lisp/auto-install/descbinds-anything.el;
# elc lisp/auto-install/ipa.el;
# elc lisp/auto-install/popwin.el;
#%end
#%m 1 1.R=\y(elc|copy) ([^\n;]+)/([^\n;]+);=#%x mylisp::$1.r|%directory%|/$2|.r|%file%|$2/$3|=
#%m 1 1.R=\y(elc|copy) ([^\n;]+);=#%x mylisp::$1.r|%directory%||.r|%file%|$2|=
#%x 1

$(dirs):
	mkdir -p $@
$(copyfiles):
	cp $< $@

.SUFFIXES: .elc .el
.el.elc:
	emacs -batch -L . -L lisp -L lisp/auto-install -eval '(package-initialize)' -f batch-byte-compile $<

all:

packages+=js2-mode
packages+=auto-complete
package-install $(MYDIR)/package-install.stamp: | $(MYDIR)
	./make_command.sh package-install $(packages)
	touch $(MYDIR)/package-install.stamp
copyfiles: $(copyfiles)
compilefiles: $(compilefiles) | $(MYDIR)/package-install.stamp
install: copyfiles compilefiles
# install: package-install
