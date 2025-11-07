.PHONY: install
install: lib

.PHONY: lib
lib:
	uv build eriklibs
	cp -vt ~/.local/share/eriks-pypi eriklibs/dist/*
