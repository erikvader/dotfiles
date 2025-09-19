# installs Font Awesome 4.7 in home

TARGETDIR := $(HOME)/.local/share/fonts
TARGETNAME := font-awesome-4.7.0
TARGET := $(TARGETDIR)/$(TARGETNAME)

.PHONY: install
install: $(TARGET)

ifdef force
.PHONY: $(TARGET)
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
