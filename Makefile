# -*- makefile-gmake -*-

all:
.PHONY: all dist install

EMACSD:=$(HOME)/.emacs.d
MYDIR:=$(EMACSD)/my

dirs+=$(MYDIR) $(MYDIR)/term
# mylisp::elc my/mwg.el
copyfiles+=$(EMACSD)/my/mwg.el
compilefiles+=$(EMACSD)/my/mwg.elc
$(EMACSD)/my/mwg.el: mwg.el | $(EMACSD)/my
$(EMACSD)/my/mwg.elc: $(EMACSD)/my/mwg.el

# mylisp::elc my/mwg-c++exp.el
copyfiles+=$(EMACSD)/my/mwg-c++exp.el
compilefiles+=$(EMACSD)/my/mwg-c++exp.elc
$(EMACSD)/my/mwg-c++exp.el: mwg-c++exp.el | $(EMACSD)/my
$(EMACSD)/my/mwg-c++exp.elc: $(EMACSD)/my/mwg-c++exp.el

# mylisp::elc my/mwg-doxygen.el
copyfiles+=$(EMACSD)/my/mwg-doxygen.el
compilefiles+=$(EMACSD)/my/mwg-doxygen.elc
$(EMACSD)/my/mwg-doxygen.el: mwg-doxygen.el | $(EMACSD)/my
$(EMACSD)/my/mwg-doxygen.elc: $(EMACSD)/my/mwg-doxygen.el

# mylisp::elc my/mwg-js2-config.el
copyfiles+=$(EMACSD)/my/mwg-js2-config.el
compilefiles+=$(EMACSD)/my/mwg-js2-config.elc
$(EMACSD)/my/mwg-js2-config.el: mwg-js2-config.el | $(EMACSD)/my
$(EMACSD)/my/mwg-js2-config.elc: $(EMACSD)/my/mwg-js2-config.el

# mylisp::elc my/ttx-mode.el
copyfiles+=$(EMACSD)/my/ttx-mode.el
compilefiles+=$(EMACSD)/my/ttx-mode.elc
$(EMACSD)/my/ttx-mode.el: ttx-mode.el | $(EMACSD)/my
$(EMACSD)/my/ttx-mode.elc: $(EMACSD)/my/ttx-mode.el

# mylisp::elc my/css-mode.el
copyfiles+=$(EMACSD)/my/css-mode.el
compilefiles+=$(EMACSD)/my/css-mode.elc
$(EMACSD)/my/css-mode.el: css-mode.el | $(EMACSD)/my
$(EMACSD)/my/css-mode.elc: $(EMACSD)/my/css-mode.el

# mylisp::elc my/term/cygwin.el
copyfiles+=$(EMACSD)/my/term/cygwin.el
compilefiles+=$(EMACSD)/my/term/cygwin.elc
$(EMACSD)/my/term/cygwin.el: term/cygwin.el | $(EMACSD)/my/term
$(EMACSD)/my/term/cygwin.elc: $(EMACSD)/my/term/cygwin.el

# mylisp::elc my/term/rosaterm.el
copyfiles+=$(EMACSD)/my/term/rosaterm.el
compilefiles+=$(EMACSD)/my/term/rosaterm.elc
$(EMACSD)/my/term/rosaterm.el: term/rosaterm.el | $(EMACSD)/my/term
$(EMACSD)/my/term/rosaterm.elc: $(EMACSD)/my/term/rosaterm.el

# mylisp::copy my/term/screen.el
copyfiles+=$(EMACSD)/my/term/screen.el
$(EMACSD)/my/term/screen.el: term/screen.el | $(EMACSD)/my/term


dirs+=$(EMACSD)/lisp $(EMACSD)/lisp/auto-install
#%x mylisp::elc.r|%directory%|/lisp|.r|%file%|lisp/csharp-mode.el|
# mylisp::elc lisp/gnuplot.el
copyfiles+=$(EMACSD)/lisp/gnuplot.el
compilefiles+=$(EMACSD)/lisp/gnuplot.elc
$(EMACSD)/lisp/gnuplot.el: lisp/gnuplot.el | $(EMACSD)/lisp
$(EMACSD)/lisp/gnuplot.elc: $(EMACSD)/lisp/gnuplot.el


# #%x mylisp::elc.r|%directory%|/lisp|.r|%file%|lisp/fuzzy.el|
# #%x mylisp::elc.r|%directory%|/lisp|.r|%file%|lisp/popup.el|
# #%x mylisp::elc.r|%directory%|/lisp|.r|%file%|lisp/auto-complete.el|
# #%x mylisp::elc.r|%directory%|/lisp|.r|%file%|lisp/auto-complete-config.el|

# #%x mylisp::elc.r|%directory%|/lisp|.r|%file%|lisp/dropdown-list.el|
# #%x mylisp::elc.r|%directory%|/lisp|.r|%file%|lisp/yasnippet.el|

# #%x mylisp::elc.r|%directory%|/lisp|.r|%file%|lisp/auto-install.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-auto-install.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-complete.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-config.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-grep.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-gtags.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-ipa.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-match-plugin.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-menu.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-migemo.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-obsolete.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-show-completion.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/anything-startup.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/descbinds-anything.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/ipa.el|
# #%x mylisp::elc.r|%directory%|/lisp/auto-install|.r|%file%|lisp/auto-install/popwin.el|

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
