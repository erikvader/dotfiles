FPATH := $(HOME)/.zshfpath.d
GITCOMP := $(FPATH)/_git

.PHONY: install
install: $(GITCOMP)

ifdef force
.PHONY: $(GITCOMP)
endif

# To get full git completions. See the install instructions in the file
# https://github.com/git/git/blob/master/contrib/completion/git-completion.zsh
$(GITCOMP):
	mkdir -p $(@D)
	ln -s /usr/share/git/completion/git-completion.zsh $@
