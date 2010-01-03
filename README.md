# Package.el

The one, the only, the legendary package.el. The magic behind ELPA.

This repository exists to polish package.el for its submission into
Emacs proper.

## TODO

* Mechanism for allowing users to upload new versions of packages they
  have created. This may tie in with the Savannah project hosting site.

* Setting the default Emacs load-path based on package.el.  This is
  tricky because we may want to let the user set package-user-dir.
  But the user may also want to refer to packages from .emacs.
  Actually, maybe this isn't even possible and we have to hard-code
  package-user-dir.  Anyway the idea is to let the user upgrade gnus
  separately, then later upgrade Emacs and have it choose the right
  one -- whichever is newer.  This implies not evalling the autoloads
  for some packages before the package stuff starts up.  Which
  packages is hard to say but at least anything that might be
  separately distributed, like gnus.

* Integrating into the Emacs build system so that the metadata for
  package.el is properly made.

* Incorporate a menu into package.el.  I have a patch from a user but I
  don't know if he has a copyright assignment.

* Prerelease version numbers.

* Add command to perform installation without marking.
