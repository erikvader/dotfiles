DIR := fzf_window_switcher
TARGET := $(INSTALLDIR)/fzf_window_switcher

ifndef INSTALLDIR
$(error INSTALLDIR not set)
endif

install: $(TARGET)

ifdef force
.PHONY: $(TARGET)
endif

$(TARGET):
	cd $(DIR); cargo build --release
	cp -t $(@D) $(DIR)/target/release/$(@F)
	cd $(DIR); cargo clean
