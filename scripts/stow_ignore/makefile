LISPS := cycle-shuf.lisp
TARGETS := $(LISPS:%.lisp=$(INSTALLDIR)/%)

ifndef INSTALLDIR
$(error INSTALLDIR not set)
endif

.PHONY: install
install: $(TARGETS)

ifdef force
.PHONY: $(TARGETS)
endif

$(INSTALLDIR)/%: lisp/%.lisp
	sbcl-build -c -o $@ $^
