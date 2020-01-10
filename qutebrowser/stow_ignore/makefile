
QUTEDIR := $(HOME)/.config/qutebrowser
QUTELOCALSHARE := $(HOME)/.local/share/qutebrowser
MYCONFIG := https://github.com/erikvader/qutebrowser-config

QUTE_DESKTOP := org.qutebrowser.qutebrowser.desktop
TORRENT_DESKTOP := deluge.desktop

.PHONY: install
install: $(QUTEDIR) xdg

$(QUTEDIR):
	git clone $(MYCONFIG) $(QUTEDIR)
	ln -s ../../../.config/qutebrowser/userscripts $(QUTELOCALSHARE)/userscripts

.PHONY: xdg
xdg:
	xdg-settings set default-web-browser $(QUTE_DESKTOP)
	xdg-mime default $(TORRENT_DESKTOP) x-scheme-handler/magnet
