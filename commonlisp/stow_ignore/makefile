QUICKLISP := $(HOME)/quicklisp/setup.lisp
QUICKLISPDOWNLOAD := /tmp/quicklisp.lisp

.PHONY: install
install: $(QUICKLISP)

ifdef force
.PHONY: $(QUICKLISP)
endif

$(QUICKLISP):
	@echo 'Downloading quicklisp...'
	curl https://beta.quicklisp.org/quicklisp.lisp -o $(QUICKLISPDOWNLOAD)
	@echo 'Installing...'
	sbcl --no-userinit --no-sysinit --non-interactive --load $(QUICKLISPDOWNLOAD) --eval '(quicklisp-quickstart:install)'
	@echo 'Cleaning up...'
	rm -f $(QUICKLISPDOWNLOAD)
