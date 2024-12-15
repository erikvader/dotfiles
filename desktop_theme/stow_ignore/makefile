LINK := $(HOME)/.local/share/icons/default/cursors
TARGET := /usr/share/icons/xcursor-breeze-snow/cursors

.PHONY: install
install: $(LINK)

#NOTE: stow doesn't like to stow absolute symlinks
#https://github.com/aspiers/stow/issues/51
$(LINK):
	ln -s $(TARGET) $(LINK)
