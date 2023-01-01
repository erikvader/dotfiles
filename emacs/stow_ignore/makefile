EMACSDIR := $(HOME)/.emacs.d
MYCONFIG := https://github.com/erikvader/.emacs.d

AURLINK := https://aur.archlinux.org/emacs28-git.git
AURCOMMIT := 70b567f060f3eaa309190b50c14d242e15a0c3bd
AURDIR := emacs-aur

.PHONY: install
install: $(EMACSDIR) $(AURDIR)

$(EMACSDIR):
	git clone --recurse-submodules $(MYCONFIG) $(EMACSDIR)

$(AURDIR):
	git clone $(AURLINK) $@
	git -C $@ checkout $(AURCOMMIT)
	git -C $@ apply ../PKGBUILD.patch
	cd $@ && makepkg --clean --install --syncdeps
