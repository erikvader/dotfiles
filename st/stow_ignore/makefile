# make install
# make install force=t
# force=t make install

BRANCH := build2
BUILDDIR := /tmp/st-build
TARGET := /usr/local/bin/st

.PHONY: install
install: $(TARGET)

ifdef force
.PHONY: $(TARGET)
endif

$(TARGET):
	@echo 'creating a new build dir'
	rm -rf $(BUILDDIR)
	mkdir -p $(BUILDDIR)
	@echo 'curling...'
	curl -L https://github.com/erikvader/st/archive/$(BRANCH).tar.gz | tar -C $(BUILDDIR) -xz --strip-components 1
	@echo 'uninstalling...'
	sudo $(MAKE) -C $(BUILDDIR) uninstall
	@echo 'building...'
	$(MAKE) -C $(BUILDDIR)
	@echo 'installing...'
	sudo $(MAKE) -C $(BUILDDIR) install
	@echo 'removing build dir'
	rm -rf $(BUILDDIR)
