QUTEDIR := $(HOME)/.config/qutebrowser
QUTELOCALSHARE := $(HOME)/.local/share/qutebrowser
MYCONFIG := https://github.com/erikvader/qutebrowser-config

QUTE_DESKTOP := org.qutebrowser.qutebrowser.desktop

.PHONY: install
install: $(QUTEDIR) xdg

$(QUTEDIR):
	git clone $(MYCONFIG) $(QUTEDIR)

.PHONY: xdg
xdg:
	xdg-settings set default-web-browser $(QUTE_DESKTOP)
