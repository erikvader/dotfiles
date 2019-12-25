
QUTEDIR := $(HOME)/.config/qutebrowser
QUTELOCALSHARE := $(HOME)/.local/share/qutebrowser
MYCONFIG := https://github.com/erikvader/qutebrowser-config

.PHONY: install
install: $(QUTEDIR)

$(QUTEDIR):
	git clone $(MYCONFIG) $(QUTEDIR)
	ln -s ../../../.config/qutebrowser/userscripts $(QUTELOCALSHARE)/userscripts
