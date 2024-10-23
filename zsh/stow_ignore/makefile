OHMYZSH := $(HOME)/.local/share/ohmyzsh
CLONEURL := https://github.com/ohmyzsh/ohmyzsh.git
REVISION := a3c579bf27b34942d4c6ad64e7cfd75788b05ea3

FPATH := $(HOME)/.zshfpath.d
GITCOMP := $(FPATH)/_git

TARGETS := $(OHMYZSH) $(GITCOMP)

install: $(TARGETS)

ifdef force
.PHONY: $(TARGETS)
endif

$(OHMYZSH):
	git clone $(CLONEURL) $@
	git -C $@ checkout $(REVISION)

# To get full git completions, see the install instructions in the file
# https://github.com/git/git/blob/master/contrib/completion/git-completion.zsh
$(GITCOMP):
	mkdir -p $(@D)
	ln -s /usr/share/git/completion/git-completion.zsh $@
