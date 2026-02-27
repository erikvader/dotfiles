XMONAD := $(INSTALLDIR)/xmonad
SRCDIR := ./xmonad
SRCS := $(SRCDIR)/xmonad.hs
STACKYAML := $(SRCDIR)/stack.yaml

AUTORANDR_DEFAULT := $(HOME)/.config/autorandr/default

ifndef INSTALLDIR
$(error INSTALLDIR not set)
endif

.PHONY: install
install: $(XMONAD) $(AUTORANDR_DEFAULT)

ifdef force
.PHONY: $(XMONAD)
endif

$(XMONAD): $(SRCS)
	stack --stack-yaml $(STACKYAML) --local-bin-path $(INSTALLDIR) install

$(AUTORANDR_DEFAULT):
	ln -fs horizontal $@
