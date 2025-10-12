QUTEDIR := $(HOME)/.config/qutebrowser
QUTELOCALSHARE := $(HOME)/.local/share/qutebrowser
MYCONFIG := https://github.com/erikvader/qutebrowser-config

QUTE_DESKTOP := org.qutebrowser.qutebrowser.desktop

.PHONY: install
install: $(QUTEDIR) xdg

ifdef force
.PHONY: $(QUTEDIR)
endif

$(QUTEDIR):
	git clone $(MYCONFIG) $(QUTEDIR)
	mkdir -p $(QUTELOCALSHARE)
	ln -s $(QUTEDIR)/js $(QUTELOCALSHARE)

.PHONY: xdg
xdg:
	xdg-settings set default-web-browser $(QUTE_DESKTOP)
	xdg-mime default $(QUTE_DESKTOP) message/rfc822
