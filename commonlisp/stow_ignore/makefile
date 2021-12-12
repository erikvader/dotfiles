QUICKLISP := $(HOME)/quicklisp/setup.lisp
QUICKLISPDOWNLOAD := /tmp/quicklisp.lisp
CORE := $(HOME)/quicklisp/script.core
GENCORE := ./create-script-core.lisp

.PHONY: install
install: $(QUICKLISP) $(CORE)

ifdef force
.PHONY: $(QUICKLISP) $(CORE)
endif

$(QUICKLISP):
	@echo 'Downloading quicklisp...'
	curl https://beta.quicklisp.org/quicklisp.lisp -o $(QUICKLISPDOWNLOAD)
	@echo 'Installing...'
	sbcl --no-userinit --no-sysinit --non-interactive --load $(QUICKLISPDOWNLOAD) --eval '(quicklisp-quickstart:install)'
	@echo 'Cleaning up...'
	rm -f $(QUICKLISPDOWNLOAD)

$(CORE): $(GENCORE)
	sbcl --script $(GENCORE) $(QUICKLISP) $(CORE)
