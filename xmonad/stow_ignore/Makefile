TARGETDIR := $(HOME)/.bin
XMONAD := $(TARGETDIR)/xmonad
SRCDIR := ./xmonad
SRCS := $(SRCDIR)/xmonad.hs
STACKYAML := $(SRCDIR)/stack.yaml

.PHONY: install
install: $(XMONAD)

ifdef force
.PHONY: $(XMONAD)
endif

$(XMONAD): $(SRCS)
	stack --stack-yaml $(STACKYAML) --local-bin-path $(TARGETDIR) install

.PHONY: restart
restart: $(XMONAD)
	xmonad --restart

.PHONY: install-devtools
install-devtools:
	stack --stack-yaml $(STACKYAML) install hlint ShellCheck hindent
