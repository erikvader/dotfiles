.PHONY: install
install: xdg cli

.PHONY: xdg
xdg:
	xdg-mime default deluge.desktop x-scheme-handler/magnet application/x-bittorrent

.PHONY: cli
cli:
	uv tool install --editable deluge-cli
