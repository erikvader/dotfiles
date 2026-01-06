# make install
# make install force=t
# force=t make install

ifndef INSTALLDIR
$(error INSTALLDIR not set)
endif

BRANCH := build3
BUILDDIR := /tmp/st-build
TARGET := $(INSTALLDIR)/st

.PHONY: install
install: $(TARGET)

ifdef force
.PHONY: $(TARGET)
endif

$(TARGET):
	@echo Creating a new build dir
	rm -rf $(BUILDDIR)
	mkdir -p $(BUILDDIR)
	@echo Curling...
	curl -L https://github.com/erikvader/st/archive/$(BRANCH).tar.gz | tar -C $(BUILDDIR) -xz --strip-components 1
	@echo 'Building...'
	$(MAKE) -C $(BUILDDIR)
	@echo Installing...
	cp -fv $(BUILDDIR)/st $(TARGET)
	@echo Not installing the manpage since I dont read it anyway

