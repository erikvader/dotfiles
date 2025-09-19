# make install
# make install force=t
# force=t make install

ifndef INSTALLDIR
$(error INSTALLDIR is not set)
endif

BUILDDIR := /tmp/sxiv-build
TARGET := $(INSTALLDIR)/sxiv

.PHONY: install
install: $(TARGET) xdg

ifdef force
.PHONY: $(TARGET)
endif

$(TARGET): config.h
	@echo 'Creating a new build dir'
	rm -rf $(BUILDDIR)
	mkdir -p $(BUILDDIR)
	@echo 'Curling...'
	curl -L https://github.com/muennich/sxiv/archive/master.tar.gz | tar -C $(BUILDDIR) -xz --strip-components 1
	@echo 'Adding my config'
	cp -t $(BUILDDIR) config.h
	@echo 'Building...'
	$(MAKE) -C $(BUILDDIR)
	@echo 'installing...'
	cp -v $(BUILDDIR)/sxiv $(TARGET)
	@echo Not installing manpage since I dont read it anyway

.PHONY: xdg
xdg:
	xdg-mime default sxiv.desktop image/png image/jpeg image/webp image/gif
