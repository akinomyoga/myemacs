# -*- makefile-gmake -*-

all:
.PHONY: all dist install

EMACSD:=$(HOME)/.emacs.d
MYDIR:=$(EMACSD)/my

#------------------------------------------------------------------------------

define MyLispCopy
copyfiles += $(EMACSD)/$1
$(EMACSD)/$1: $(1:my/%=%) | $(patsubst %/,%,$(dir $(EMACSD)/$1))
endef

define MyLispCompile
copyfiles += $(EMACSD)/$1
compilefiles += $(EMACSD)/$1c
$(EMACSD)/$1: $(1:my/%=%) | $(patsubst %/,%,$(dir $(EMACSD)/$1))
$(EMACSD)/$1c: $(EMACSD)/$1
endef

#------------------------------------------------------------------------------

dirs += $(MYDIR) $(MYDIR)/term
$(eval $(call MyLispCompile,my/mwg.el))
$(eval $(call MyLispCompile,my/mwg-decode-map.el))
$(eval $(call MyLispCompile,my/mwg-c++exp.el))
$(eval $(call MyLispCompile,my/mwg-doxygen.el))
$(eval $(call MyLispCompile,my/mwg-js2-config.el))
$(eval $(call MyLispCompile,my/ttx-mode.el))
$(eval $(call MyLispCompile,my/css-mode.el))
$(eval $(call MyLispCompile,my/term/cygwin.el))
$(eval $(call MyLispCompile,my/term/rosaterm.el))
$(eval $(call MyLispCopy,my/term/screen.el))
$(eval $(call MyLispCopy,my/term/screen.xterm.el))

#dirs += $(EMACSD)/lisp $(EMACSD)/lisp/auto-install
#$(eval $(call MyLispCompile,lisp/csharp-mode.el))

# $(eval $(call MyLispCompile,lisp/fuzzy.el))
# $(eval $(call MyLispCompile,lisp/popup.el))
# $(eval $(call MyLispCompile,lisp/auto-complete.el))
# $(eval $(call MyLispCompile,lisp/auto-complete-config.el))

# $(eval $(call MyLispCompile,lisp/dropdown-list.el))
# $(eval $(call MyLispCompile,lisp/yasnippet.el))

# $(eval $(call MyLispCompile,lisp/auto-install.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-auto-install.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-complete.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-config.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-grep.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-gtags.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-ipa.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-match-plugin.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-menu.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-migemo.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-obsolete.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-show-completion.el))
# $(eval $(call MyLispCompile,lisp/auto-install/anything-startup.el))
# $(eval $(call MyLispCompile,lisp/auto-install/descbinds-anything.el))
# $(eval $(call MyLispCompile,lisp/auto-install/ipa.el))
# $(eval $(call MyLispCompile,lisp/auto-install/popwin.el))

$(dirs):
	mkdir -p $@
$(copyfiles):
	cp $< $@

.SUFFIXES: .elc .el
.el.elc:
	emacs -batch -L . -L lisp -L lisp/auto-install -eval '(package-initialize)' -f batch-byte-compile $<

all:

packages += js2-mode
packages += auto-complete
packages += markdown-mode
package-install $(MYDIR)/package-install.stamp: | $(MYDIR)
	./make_command.sh package-install $(packages)
	touch $(MYDIR)/package-install.stamp
copyfiles: $(copyfiles)
compilefiles: $(compilefiles) | $(MYDIR)/package-install.stamp
install: copyfiles compilefiles
# install: package-install
