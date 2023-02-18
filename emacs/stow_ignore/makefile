EMACSDIR := $(HOME)/.emacs.d
MYCONFIG := https://github.com/erikvader/.emacs.d

.PHONY: install
install: $(EMACSDIR)

$(EMACSDIR):
	git clone --recurse-submodules $(MYCONFIG) $(EMACSDIR)

