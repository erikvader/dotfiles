TARGET := $(HOME)/.local/share/ohmyzsh
CLONEURL := https://github.com/ohmyzsh/ohmyzsh.git
REVISION := a3c579bf27b34942d4c6ad64e7cfd75788b05ea3

install: $(TARGET)

ifdef force
.PHONY: $(TARGET)
endif

$(TARGET):
	git clone $(CLONEURL) $@
	cd $(TARGET) && git checkout $(REVISION)
