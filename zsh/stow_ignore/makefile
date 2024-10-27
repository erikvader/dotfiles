OHMYZSH := $(HOME)/.local/share/ohmyzsh
CLONEURL := https://github.com/ohmyzsh/ohmyzsh.git
REVISION := a3c579bf27b34942d4c6ad64e7cfd75788b05ea3

install: $(OHMYZSH)

ifdef force
.PHONY: $(OHMYZSH)
endif

$(OHMYZSH):
	git clone $(CLONEURL) $@
	git -C $@ checkout $(REVISION)
