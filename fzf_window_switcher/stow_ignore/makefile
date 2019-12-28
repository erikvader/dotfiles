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
	cargo build --release --manifest-path $(DIR)/Cargo.toml
	cp -t $(@D) $(DIR)/target/release/$(@F)
	cargo clean --manifest-path $(DIR)/Cargo.toml
