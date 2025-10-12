.PHONY: install
install:
	uv build eriklibs
	cp -vt ~/.local/share/eriks-pypi eriklibs/dist/*
	uv tool install eriklibs
