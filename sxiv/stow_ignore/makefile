
BUILDDIR = /tmp/sxiv-build
TARGET = /usr/local/bin/sxiv

.PHONY: install
install: $(TARGET)

$(TARGET):
	@echo 'creating a new build dir'
	rm -rf $(BUILDDIR)
	mkdir -p $(BUILDDIR)
	@echo 'curling...'
	curl -L https://github.com/muennich/sxiv/archive/master.tar.gz | tar -C $(BUILDDIR) -xz --strip-components 1
	@echo 'linking in config'
	ln -srt $(BUILDDIR) config.h
	@echo 'building...'
	$(MAKE) -C $(BUILDDIR)
	@echo 'installing...'
	sudo $(MAKE) -C $(BUILDDIR) install
	@echo 'removing build dir'
	rm -rf $(BUILDDIR)
