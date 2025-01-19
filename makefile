# package manager commands
PAC := yay
PACLIST := $(PAC) -Qq
PACINSTALL := $(PAC) --noconfirm -S
ALLPAC := /tmp/dotfiles_make_all_pac
PIPLIST := pipx list --short | cut -d' ' -f1
PIPINSTALL := pipx install
ALLPIP := /tmp/dotfiles_make_all_pip

IGNOREDIR := stow_ignore
STOWFLAGS := --ignore='^$(IGNOREDIR)$$' --ignore='\.dir-locals\.el$$' --no-folding

export INSTALLDIR := $(HOME)/.bin

# $(call maybe-install,package1 package2 ...)
maybe-install = $(call install,$(call not-installed,$1,$2),$2)
install = $(if $1,$($2INSTALL) $1,@echo packages '($2)' already installed for $(patsubst %:install,%,$@))
not-installed = $(filter-out $(filter $1,$(shell $($2LIST))),$1)
install-from = $(if $(wildcard $1),$(call maybe-install,$(shell grep -v '^#' "$1"),$2),)

maybe-make = $(if $(wildcard $1/[Mm]akefile),$(MAKE) -C $1 $2,)

FINDIGNORE := \( -name other -o -name '.git' \)
dirs := $(shell find . -maxdepth 1 -mindepth 1 $(FINDIGNORE) -prune -o -type d -printf '%P ')
dirsadd := $(addsuffix \:add,$(dirs))
dirsdel := $(addsuffix \:del,$(dirs))
dirsre := $(addsuffix \:re,$(dirs))
dirsdry := $(addsuffix \:dry,$(dirs))
dirsinstall := $(addsuffix \:install,$(dirs))
dirsmake := $(addsuffix \:make,$(dirs))
dirsmakeadd := $(addsuffix \:makeadd,$(dirs))

.PHONY: $(dirsinstall) $(dirsmake) $(dirsadd) $(dirsmakeadd) $(dirsdry) $(dirs) $(dirsdel) $(dirsre) all help install-everything remove-dead-links

help:
	@echo 'My dotfiles manager'
	@echo 'Each package is a directory. Everything will be stowed except for package/$(IGNOREDIR)/'
	@echo 'This directory can contain these special files:'
	@echo '  packages:    packages installed with $(PACINSTALL), one per line'
	@echo '  pippackages: packages installed with $(PIPINSTALL), one per line'
	@echo '  makefile:    should have the target "install" which should install the package'
	@echo
	@echo 'Usage:'
	@echo 'make all             to install, make and stow everything'
	@echo 'make package:add     to stow a package'
	@echo 'make package:del     to remove stowed package'
	@echo 'make package:re      to restow package (delete and stow)'
	@echo 'make package:dry     to stow and see what happens'
	@echo 'make package:install to install required packages (pip and $(PAC))'
	@echo 'make package:make    to run "make install" on package'
	@echo 'make package         to install, make and then stow'
	@echo
	@echo 'Available packages:'
	@echo $(dirs)
	@echo
	@echo 'Notes: Might not work with spaces in filenames at all!'
	@echo '       GNU stow must be installed'

all: install-everything $(dirsmakeadd) remove-dead-links

$(dirsmakeadd): %\:makeadd: %\:make %\:add
$(dirs): %: %\:install %\:makeadd

$(dirsinstall):
	$(call install-from,$(patsubst %:install,%,$@)/$(IGNOREDIR)/packages,PAC)
	$(call install-from,$(patsubst %:install,%,$@)/$(IGNOREDIR)/pippackages,PIP)

$(dirsmake):
	$(call maybe-make,$(patsubst %:make,%,$@)/$(IGNOREDIR),install)

$(dirsadd):
	stow $(STOWFLAGS) $(patsubst %:add,%,$@)

$(dirsre): STOWFLAGS += --restow
$(dirsre): %\:re: %\:add

$(dirsdry): STOWFLAGS += --no
$(dirsdry): %\:dry: %\:add

$(dirsdel): STOWFLAGS += --delete
$(dirsdel): %\:del: %\:add

.INTERMEDIATE: $(ALLPAC) $(ALLPIP)

define build_packages_file_template
$1:
	@echo finding all $2 ...
	@find $(dirs) -type f -path '*/$$(IGNOREDIR)/$2' -exec awk 1 {} + | sort -u > $$@
endef

$(eval $(call build_packages_file_template,$(ALLPAC),packages))
$(eval $(call build_packages_file_template,$(ALLPIP),pippackages))

install-everything: $(ALLPAC) $(ALLPIP)
	$(call install-from,$(ALLPAC),PAC)
	$(call install-from,$(ALLPIP),PIP)

remove-dead-links:
	find $(INSTALLDIR) -mindepth 1 -maxdepth 1 -xtype l -print -delete
