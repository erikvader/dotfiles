# make install
# make install force=t
# force=t make install

BUILDDIR := /tmp/sxiv-build
TARGET := /usr/local/bin/sxiv

.PHONY: install
install: $(TARGET) xdg

ifdef force
.PHONY: $(TARGET)
endif

$(TARGET):
	@echo 'creating a new build dir'
	rm -rf $(BUILDDIR)
	mkdir -p $(BUILDDIR)
	@echo 'curling...'
	curl -L https://github.com/muennich/sxiv/archive/master.tar.gz | tar -C $(BUILDDIR) -xz --strip-components 1
	@echo 'uninstalling...'
	sudo $(MAKE) -C $(BUILDDIR) uninstall
	@echo 'linking in config'
	cp -t $(BUILDDIR) config.h
	@echo 'building...'
	$(MAKE) -C $(BUILDDIR)
	@echo 'installing...'
	sudo $(MAKE) -C $(BUILDDIR) install
	@echo 'removing build dir'
	rm -rf $(BUILDDIR)

.PHONY: xdg
xdg:
	xdg-mime default sxiv.desktop image/png image/jpeg image/webp image/gif
