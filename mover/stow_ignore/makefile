DIR := mover
TARGET := $(INSTALLDIR)/mover

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
