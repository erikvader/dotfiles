# -*- tab-width: 4; -*-

# package manager commands
PAC := yay
PACLIST := $(PAC) -Qq
PACINSTALL := $(PAC) -S
PIPLIST := pip list --user --format freeze | sed 's/==.*$$//'
PIPINSTALL := pip install --user

IGNOREDIR := stow_ignore
STOWFLAGS := --ignore='^$(IGNOREDIR)$$'

# $(call maybe-install,package1 package2 ...)
maybe-install = $(call install,$(call not-installed,$1,$2),$2)
install = $(if $1,$($2INSTALL) $1,@echo packages '($2)' already installed for $(patsubst %:install,%,$@))
not-installed = $(filter-out $(filter $1,$(shell $($2LIST))),$1)
install-from = $(if $(wildcard $1),$(call maybe-install,$(shell cat "$1"),$2),)

maybe-make = $(if $(wildcard $1/[Mm]akefile),$(MAKE) -C $1 $2,)

FINDIGNORE := \( -name other -o -name '.git' \)
dirs := $(shell find . -maxdepth 1 -mindepth 1 $(FINDIGNORE) -prune -o -type d -printf '%P ')
dirsadd := $(addsuffix \:add,$(dirs))
dirsdel := $(addsuffix \:del,$(dirs))
dirsre := $(addsuffix \:re,$(dirs))
dirsdry := $(addsuffix \:dry,$(dirs))
dirsinstall := $(addsuffix \:install,$(dirs))

.PHONY: $(dirsinstall) $(dirsadd) $(dirsdry) $(dirs) $(dirsdel) $(dirsre) all help

help:
	@echo 'My dotfiles manager'
	@echo 'Each package is a directory. Everything will be stowed except for package/$(IGNOREDIR)/'
	@echo 'This directory can contain these special files:'
	@echo '  packages: packages installed with $(PACINSTALL), one per line'
	@echo '  makefile: should have the target "install" which should install the package'
	@echo
	@echo 'Usage:'
	@echo 'make package:add     to stow everything'
	@echo 'make package:del     to remove stowed package'
	@echo 'make package:re      to restow package (delete and stow)'
	@echo 'make package:dry     to stow and see what happens'
	@echo 'make package:install to install required packages'
	@echo 'make package         to install and then stow'
	@echo
	@echo 'Available packages:'
	@echo $(dirs)
	@echo
	@echo 'Notes: Might not work with spaces in filenames at all!'
	@echo '       GNU stow must be installed'

all: $(dirs)

$(dirs): %: %\:install %\:add

$(dirsinstall):
	$(call install-from,$(patsubst %:install,%,$@)/$(IGNOREDIR)/packages,PAC)
	$(call install-from,$(patsubst %:install,%,$@)/$(IGNOREDIR)/pippackages,PIP)
	$(call maybe-make,$(patsubst %:install,%,$@)/$(IGNOREDIR),install)

$(dirsadd):
	stow $(STOWFLAGS) $(patsubst %:add,%,$@)

$(dirsre): STOWFLAGS += --restow
$(dirsre): %\:re: %\:add

$(dirsdry): STOWFLAGS += --no
$(dirsdry): %\:dry: %\:add

$(dirsdel): STOWFLAGS += --delete
$(dirsdel): %\:del: %\:add

conky\:add: m4\:add
conky\:install: m4\:install
xmonad\:add: STOWFLAGS += --no-folding
display_updater\:add: STOWFLAGS += --no-folding
