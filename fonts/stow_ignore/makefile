# installs Font Awesome 4.7 in home

TARGETDIR := $(HOME)/.local/share/fonts
TARGETNAME := font-awesome-4.7.0
TARGET := $(TARGETDIR)/$(TARGETNAME)
SUBPIXEL := $(HOME)/.config/fontconfig/conf.d/10-sub-pixel-rgb.conf

.PHONY: install
install: $(TARGET) $(SUBPIXEL)

ifdef force
.PHONY: $(TARGET) $(SUBPIXEL)
endif

$(TARGET):
	mkdir -p $(TARGETDIR)
	@echo downloading Font awesome 4...
	curl -L https://fontawesome.com/v4.7.0/assets/font-awesome-4.7.0.zip > '$(TARGET).zip'
	@echo removing potentially existing old folder...
	rm -rf '$(TARGET)'
	@echo unzipping...
	unzip -d '$(TARGETDIR)' '$(TARGET).zip'
	@echo removing downloaded zip...
	rm '$(TARGET).zip'

$(SUBPIXEL):
	mkdir -p $(@D)
	ln -s /usr/share/fontconfig/conf.avail/10-sub-pixel-rgb.conf $@
