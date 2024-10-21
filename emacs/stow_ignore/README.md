# Introduction
Install a version of the official `emacs-nativecomp` without toolkit scrollbars.

# Download sources
First, get the `PKGBUILD` used by the official emacs packages on Arch.
Read more [here](https://wiki.archlinux.org/title/Arch_Build_System).

```sh
git clone --branch 28.2-2 https://gitlab.archlinux.org/archlinux/packaging/packages/emacs.git
```

# Modify PKGBUILD
There are several optional modifications that can be made.

## Make sure only the nativecomp variant is built
Remove the other calls to `make` and `configure` in the
same function, keep the nativecomp ones. Add `-jN` to the nativecomp
`make` call where `N` is the number of cores on your system. Also
remove the bodies of the package functions, except for nativecomp.

## Add desired customizations
Add the flag `--without-toolkit-scroll-bars` to the nativecomp step in
`build()`. This will make emacs use its own scrollbars, removing the
pain of having them get changed based on the global system theme.

## This is what the diff could look like
```diff
--- old/emacs/PKGBUILD
+++ new/emacs/PKGBUILD
@@ -45,7 +45,6 @@
   zlib
 )
 makedepends=(libgccjit)
-options=(debug)
 source=(https://ftp.gnu.org/gnu/emacs/${pkgname}-${pkgver}.tar.xz{,.sig})
 b2sums=('a7e4990658b5e7306510f8dded93aaf0b82cdd9306df8b786526d038c3249ef9579287075f2235eb01a71ae1699db555254f137b86ab2d2305b45895053df552'
         'SKIP')
@@ -70,37 +69,20 @@
   export ac_cv_lib_gif_EGifPutExtensionLast=yes
 
   cd ${pkgname}-${pkgver}
-  ./configure $_confflags \
-    --with-x-toolkit=gtk3
-  make
 
   cd ../${pkgbase}-${pkgver}-nativecomp
   ./configure \
     --with-x-toolkit=gtk3 \
+    --without-toolkit-scroll-bars \
     --with-native-compilation \
     $_confflags
-  make NATIVE_FULL_AOT=1 bootstrap
+  make NATIVE_FULL_AOT=1 -j6 bootstrap
 
   cd ../${pkgbase}-${pkgver}-nox
-  ./configure \
-    --without-x \
-    --without-sound \
-    $_confflags
-  make
 }
 
 package_emacs() {
   pkgdesc='The extensible, customizable, self-documenting real-time display editor'
-
-  cd ${pkgname}-${pkgver}
-  make DESTDIR="${pkgdir}" install
-
-  # remove conflict with ctags package
-  mv "${pkgdir}"/usr/bin/{ctags,ctags.emacs}
-  mv "${pkgdir}"/usr/share/man/man1/{ctags.1.gz,ctags.emacs.1}
-
-  # fix user/root permissions on usr/share files
-  find "${pkgdir}"/usr/share/emacs/${pkgver} -exec chown root:root {} \;
 }
 
 package_emacs-nativecomp() {
@@ -122,30 +104,4 @@
 
 package_emacs-nox() {
   pkgdesc='The extensible, customizable, self-documenting real-time display editor without X11 support'
-  depends=(
-    gmp
-    gnutls
-    hicolor-icon-theme
-    jansson
-    lcms2
-    libacl.so
-    libdbus-1.so
-    libgpm.so
-    libncursesw.so
-    libsystemd.so
-    libxml2.so
-    zlib
-  )
-  provides=(emacs)
-  conflicts=(emacs)
-
-  cd ${pkgbase}-${pkgver}-nox
-  make DESTDIR="${pkgdir}" install
-
-  # remove conflict with ctags package
-  mv "${pkgdir}"/usr/bin/{ctags,ctags.emacs}
-  mv "${pkgdir}"/usr/share/man/man1/{ctags.1.gz,ctags.emacs.1}
-
-  # fix user/root permissions on usr/share files
-  find "${pkgdir}"/usr/share/emacs/${pkgver} -exec chown root:root {} \;
 }
```

# Build
```sh
makepkg --clean --syncdeps --skippgpcheck
```

## PGP
An alternative to skipping the PGP check above is to import the key
of the maintainer listed in the `PKGBUILD`:
```sh
pgp --recv-key <the long fingerprint>
```
The keyrings used by pacman are [separate](http://allanmcrae.com/2015/01/two-pgp-keyrings-for-package-management-in-arch-linux/)
from the user, which is why the maintainer's key needs to be imported.

# Install
```sh
pacman -U emacs-nativecomp-28.2-2-x86_64.pkg.tar.zst
```

Don't forget to add `emacs-nativecomp` to the ignore list in
`/etc/pacman.conf` to avoid pacman updating emacs to another version
with different settings.

```
IgnorePkg = emacs-nativecomp
```

# Installing on a restricted system
These are solutions to steps that can cause problems when building on
an old and/or restricted system. These steps doesn't use `makepkg`, but
uses the source directly from
[savannah](http://git.savannah.gnu.org/cgit/emacs.git) 
or [ftp](http://ftp.gnu.org/gnu/emacs/).

## Avoid building the info pages
Can't for some reason install
[makeinfo](https://www.gnu.org/software/texinfo/)? Its possible to
skip that step, but it's a little fiddly.

Before `configure`, run `mkdir -p info/emacs` to make the configure
script believe there already is documention included (its included in
releases, but not from the source repo or something).

Then when running `make`, add the argument `MAKEINFO=true` to use the
program `true` instead of `makeinfo`, basically making it into a
no-op.

## Set install location
Install to another location if missing root access.

```sh
./configure --prefix="$HOME/emacs"
```

## Skip imaging and networking stuff
Can't for some reason install some optional libraries? Exclude them in
the configure step. The configure script will say what it couldn't
find and suggest flags by itself too.

```sh
./configure --with-gnutls=ifavailable --with-jpeg=ifavailable
```

## Compile and install
Follow the instructions in the `INSTALL` file (or copy from `PKGBUILD`),
but they basically are:

```sh
./configure --without-toolkit-toolbars
make -j6
make install
```

Most default values for the configure script are fine, it's not necessary
to copy all of the ones in the `PKGBUILD`.
