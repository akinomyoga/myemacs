# -*- makefile-gmake -*-

all:
.PHONY: all dist install

EMACSD:=$(HOME)/.emacs.d
MYDIR:=$(EMACSD)/my

dirs+=$(MYDIR)
copyfiles+=$(MYDIR)/mwg.el
compilefiles+=$(MYDIR)/mwg.elc
$(MYDIR)/mwg.el: mwg.el | $(MYDIR)
$(MYDIR)/mwg.elc: $(MYDIR)/mwg.el
copyfiles+=$(MYDIR)/mwg-c++exp.el
compilefiles+=$(MYDIR)/mwg-c++exp.elc
$(MYDIR)/mwg-c++exp.el: mwg-c++exp.el | $(MYDIR)
$(MYDIR)/mwg-c++exp.elc: $(MYDIR)/mwg-c++exp.el
copyfiles+=$(MYDIR)/mwg-doxygen.el
compilefiles+=$(MYDIR)/mwg-doxygen.elc
$(MYDIR)/mwg-doxygen.el: mwg-doxygen.el | $(MYDIR)
$(MYDIR)/mwg-doxygen.elc: $(MYDIR)/mwg-doxygen.el
copyfiles+=$(MYDIR)/mwg-js2-config.el
compilefiles+=$(MYDIR)/mwg-js2-config.elc
$(MYDIR)/mwg-js2-config.el: mwg-js2-config.el | $(MYDIR)
$(MYDIR)/mwg-js2-config.elc: $(MYDIR)/mwg-js2-config.el
copyfiles+=$(MYDIR)/ttx-mode.el
compilefiles+=$(MYDIR)/ttx-mode.elc
$(MYDIR)/ttx-mode.el: ttx-mode.el | $(MYDIR)
$(MYDIR)/ttx-mode.elc: $(MYDIR)/ttx-mode.el
copyfiles+=$(MYDIR)/css-mode.el
compilefiles+=$(MYDIR)/css-mode.elc
$(MYDIR)/css-mode.el: css-mode.el | $(MYDIR)
$(MYDIR)/css-mode.elc: $(MYDIR)/css-mode.el

dirs+=$(MYDIR)/term
copyfiles+=$(MYDIR)/term/cygwin.el
compilefiles+=$(MYDIR)/term/cygwin.elc
$(MYDIR)/term/cygwin.el: term/cygwin.el | $(MYDIR)/term
$(MYDIR)/term/cygwin.elc: $(MYDIR)/term/cygwin.el
copyfiles+=$(MYDIR)/term/rosaterm.el
compilefiles+=$(MYDIR)/term/rosaterm.elc
$(MYDIR)/term/rosaterm.el: term/rosaterm.el | $(MYDIR)/term
$(MYDIR)/term/rosaterm.elc: $(MYDIR)/term/rosaterm.el
copyfiles+=$(MYDIR)/term/screen.el
$(MYDIR)/term/screen.el: term/screen.el | $(MYDIR)/term

dirs+=$(EMACSD)/lisp $(EMACSD)/lisp/auto-install
copyfiles+=$(EMACSD)/lisp/dropdown-list.el
compilefiles+=$(EMACSD)/lisp/dropdown-list.elc
$(EMACSD)/lisp/dropdown-list.el: lisp/dropdown-list.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/dropdown-list.elc: $(EMACSD)/lisp/dropdown-list.el
copyfiles+=$(EMACSD)/lisp/fuzzy.el
compilefiles+=$(EMACSD)/lisp/fuzzy.elc
$(EMACSD)/lisp/fuzzy.el: lisp/fuzzy.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/fuzzy.elc: $(EMACSD)/lisp/fuzzy.el
copyfiles+=$(EMACSD)/lisp/popup.el
compilefiles+=$(EMACSD)/lisp/popup.elc
$(EMACSD)/lisp/popup.el: lisp/popup.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/popup.elc: $(EMACSD)/lisp/popup.el
copyfiles+=$(EMACSD)/lisp/auto-complete.el
compilefiles+=$(EMACSD)/lisp/auto-complete.elc
$(EMACSD)/lisp/auto-complete.el: lisp/auto-complete.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-complete.elc: $(EMACSD)/lisp/auto-complete.el
copyfiles+=$(EMACSD)/lisp/auto-complete-config.el
compilefiles+=$(EMACSD)/lisp/auto-complete-config.elc
$(EMACSD)/lisp/auto-complete-config.el: lisp/auto-complete-config.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-complete-config.elc: $(EMACSD)/lisp/auto-complete-config.el
copyfiles+=$(EMACSD)/lisp/auto-install.el
compilefiles+=$(EMACSD)/lisp/auto-install.elc
$(EMACSD)/lisp/auto-install.el: lisp/auto-install.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install.elc: $(EMACSD)/lisp/auto-install.el
copyfiles+=$(EMACSD)/lisp/csharp-mode.el
compilefiles+=$(EMACSD)/lisp/csharp-mode.elc
$(EMACSD)/lisp/csharp-mode.el: lisp/csharp-mode.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/csharp-mode.elc: $(EMACSD)/lisp/csharp-mode.el
copyfiles+=$(EMACSD)/lisp/gnuplot.el
compilefiles+=$(EMACSD)/lisp/gnuplot.elc
$(EMACSD)/lisp/gnuplot.el: lisp/gnuplot.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/gnuplot.elc: $(EMACSD)/lisp/gnuplot.el
copyfiles+=$(EMACSD)/lisp/yasnippet.el
compilefiles+=$(EMACSD)/lisp/yasnippet.elc
$(EMACSD)/lisp/yasnippet.el: lisp/yasnippet.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/yasnippet.elc: $(EMACSD)/lisp/yasnippet.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything.elc
$(EMACSD)/lisp/auto-install/anything.el: lisp/auto-install/anything.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything.elc: $(EMACSD)/lisp/auto-install/anything.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-auto-install.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-auto-install.elc
$(EMACSD)/lisp/auto-install/anything-auto-install.el: lisp/auto-install/anything-auto-install.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-auto-install.elc: $(EMACSD)/lisp/auto-install/anything-auto-install.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-complete.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-complete.elc
$(EMACSD)/lisp/auto-install/anything-complete.el: lisp/auto-install/anything-complete.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-complete.elc: $(EMACSD)/lisp/auto-install/anything-complete.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-config.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-config.elc
$(EMACSD)/lisp/auto-install/anything-config.el: lisp/auto-install/anything-config.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-config.elc: $(EMACSD)/lisp/auto-install/anything-config.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-grep.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-grep.elc
$(EMACSD)/lisp/auto-install/anything-grep.el: lisp/auto-install/anything-grep.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-grep.elc: $(EMACSD)/lisp/auto-install/anything-grep.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-gtags.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-gtags.elc
$(EMACSD)/lisp/auto-install/anything-gtags.el: lisp/auto-install/anything-gtags.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-gtags.elc: $(EMACSD)/lisp/auto-install/anything-gtags.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-ipa.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-ipa.elc
$(EMACSD)/lisp/auto-install/anything-ipa.el: lisp/auto-install/anything-ipa.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-ipa.elc: $(EMACSD)/lisp/auto-install/anything-ipa.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-match-plugin.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-match-plugin.elc
$(EMACSD)/lisp/auto-install/anything-match-plugin.el: lisp/auto-install/anything-match-plugin.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-match-plugin.elc: $(EMACSD)/lisp/auto-install/anything-match-plugin.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-menu.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-menu.elc
$(EMACSD)/lisp/auto-install/anything-menu.el: lisp/auto-install/anything-menu.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-menu.elc: $(EMACSD)/lisp/auto-install/anything-menu.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-migemo.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-migemo.elc
$(EMACSD)/lisp/auto-install/anything-migemo.el: lisp/auto-install/anything-migemo.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-migemo.elc: $(EMACSD)/lisp/auto-install/anything-migemo.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-obsolete.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-obsolete.elc
$(EMACSD)/lisp/auto-install/anything-obsolete.el: lisp/auto-install/anything-obsolete.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-obsolete.elc: $(EMACSD)/lisp/auto-install/anything-obsolete.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-show-completion.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-show-completion.elc
$(EMACSD)/lisp/auto-install/anything-show-completion.el: lisp/auto-install/anything-show-completion.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-show-completion.elc: $(EMACSD)/lisp/auto-install/anything-show-completion.el
copyfiles+=$(EMACSD)/lisp/auto-install/anything-startup.el
compilefiles+=$(EMACSD)/lisp/auto-install/anything-startup.elc
$(EMACSD)/lisp/auto-install/anything-startup.el: lisp/auto-install/anything-startup.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/anything-startup.elc: $(EMACSD)/lisp/auto-install/anything-startup.el
copyfiles+=$(EMACSD)/lisp/auto-install/descbinds-anything.el
compilefiles+=$(EMACSD)/lisp/auto-install/descbinds-anything.elc
$(EMACSD)/lisp/auto-install/descbinds-anything.el: lisp/auto-install/descbinds-anything.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/descbinds-anything.elc: $(EMACSD)/lisp/auto-install/descbinds-anything.el
copyfiles+=$(EMACSD)/lisp/auto-install/ipa.el
compilefiles+=$(EMACSD)/lisp/auto-install/ipa.elc
$(EMACSD)/lisp/auto-install/ipa.el: lisp/auto-install/ipa.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/ipa.elc: $(EMACSD)/lisp/auto-install/ipa.el
copyfiles+=$(EMACSD)/lisp/auto-install/popwin.el
compilefiles+=$(EMACSD)/lisp/auto-install/popwin.elc
$(EMACSD)/lisp/auto-install/popwin.el: lisp/auto-install/popwin.el | $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
$(EMACSD)/lisp/auto-install/popwin.elc: $(EMACSD)/lisp/auto-install/popwin.el

dirs+=$(EMACSD)/lisp/ac-dict
Makefile: lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/ada-mode
$(EMACSD)/lisp/ac-dict/ada-mode: lisp/ac-dict/ada-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/c++-mode
$(EMACSD)/lisp/ac-dict/c++-mode: lisp/ac-dict/c++-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/caml-mode
$(EMACSD)/lisp/ac-dict/caml-mode: lisp/ac-dict/caml-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/clojure-mode
$(EMACSD)/lisp/ac-dict/clojure-mode: lisp/ac-dict/clojure-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/clojurescript-mode
$(EMACSD)/lisp/ac-dict/clojurescript-mode: lisp/ac-dict/clojurescript-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/c-mode
$(EMACSD)/lisp/ac-dict/c-mode: lisp/ac-dict/c-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/coq-mode
$(EMACSD)/lisp/ac-dict/coq-mode: lisp/ac-dict/coq-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/css-mode
$(EMACSD)/lisp/ac-dict/css-mode: lisp/ac-dict/css-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/erlang-mode
$(EMACSD)/lisp/ac-dict/erlang-mode: lisp/ac-dict/erlang-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/go-mode
$(EMACSD)/lisp/ac-dict/go-mode: lisp/ac-dict/go-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/haskell-mode
$(EMACSD)/lisp/ac-dict/haskell-mode: lisp/ac-dict/haskell-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/java-mode
$(EMACSD)/lisp/ac-dict/java-mode: lisp/ac-dict/java-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/javascript-mode
$(EMACSD)/lisp/ac-dict/javascript-mode: lisp/ac-dict/javascript-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/latex-mode
$(EMACSD)/lisp/ac-dict/latex-mode: lisp/ac-dict/latex-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/lua-mode
$(EMACSD)/lisp/ac-dict/lua-mode: lisp/ac-dict/lua-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/php-mode
$(EMACSD)/lisp/ac-dict/php-mode: lisp/ac-dict/php-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/python-mode
$(EMACSD)/lisp/ac-dict/python-mode: lisp/ac-dict/python-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/ruby-mode
$(EMACSD)/lisp/ac-dict/ruby-mode: lisp/ac-dict/ruby-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/scheme-mode
$(EMACSD)/lisp/ac-dict/scheme-mode: lisp/ac-dict/scheme-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/sclang-mode
$(EMACSD)/lisp/ac-dict/sclang-mode: lisp/ac-dict/sclang-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/sh-mode
$(EMACSD)/lisp/ac-dict/sh-mode: lisp/ac-dict/sh-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/tcl-mode
$(EMACSD)/lisp/ac-dict/tcl-mode: lisp/ac-dict/tcl-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/tex-mode
$(EMACSD)/lisp/ac-dict/tex-mode: lisp/ac-dict/tex-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/ts-mode
$(EMACSD)/lisp/ac-dict/ts-mode: lisp/ac-dict/ts-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/tuareg-mode
$(EMACSD)/lisp/ac-dict/tuareg-mode: lisp/ac-dict/tuareg-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict
copyfiles+=$(EMACSD)/lisp/ac-dict/verilog-mode
$(EMACSD)/lisp/ac-dict/verilog-mode: lisp/ac-dict/verilog-mode | $(EMACSD)/lisp $(EMACSD)/lisp/ac-dict

$(dirs):
	mkdir -p $@
$(copyfiles):
	cp -p $< $@

.SUFFIXES: .elc .el
.el.elc:
	emacs -batch -L . -L lisp -L lisp/auto-install -L $(EMACSD)/elpa/js2-mode-*/ -f batch-byte-compile $<

all:
install: $(copyfiles) $(compilefiles)
