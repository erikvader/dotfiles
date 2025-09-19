LINK := $(HOME)/.local/share/icons/default
TARGET := /usr/share/icons/Breeze_Light

.PHONY: install
install: $(LINK)

#NOTE: stow doesn't like to stow absolute symlinks
#https://github.com/aspiers/stow/issues/51
$(LINK):
	mkdir -p $(@D)
	ln -vsf $(TARGET) $(LINK)
