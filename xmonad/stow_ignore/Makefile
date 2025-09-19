XMONAD := $(INSTALLDIR)/xmonad
SRCDIR := ./xmonad
SRCS := $(SRCDIR)/xmonad.hs
STACKYAML := $(SRCDIR)/stack.yaml

ifndef INSTALLDIR
$(error INSTALLDIR not set)
endif

.PHONY: install
install: $(XMONAD)

ifdef force
.PHONY: $(XMONAD)
endif

$(XMONAD): $(SRCS)
	stack --stack-yaml $(STACKYAML) --local-bin-path $(INSTALLDIR) install
