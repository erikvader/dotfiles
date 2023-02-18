# Introduction
Install a version of the official `emacs-nativecomp` without toolkit scrollbars.

# Download sources
First, get the `PKGBUILD` used by the official emacs packages on Arch.
Read more [here](https://wiki.archlinux.org/title/Arch_Build_System).

```sh
asp update emacs
asp export extra/emacs-nativecomp
```

# Modify PKGBUILD
Add the flag `--without-toolkit-scroll-bars` to the nativecomp step in `build()`. Remove
the calls to `make` and `configure` in the same function. Add `-jN` to the
nativecomp `make` call where `N` is the number of cores on your system.
Also remove the bodies of the package functions, except for nativecomp.

# Build
```sh
makepkg --clean --syncdeps --skippgpcheck
```

# Install
```sh
pacman -U emacs-nativecomp-28.2-2-x86_64.pkg.tar.zst
```
