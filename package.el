;;; package.el --- Simple package system for Emacs

;; Copyright (C) 2007, 2008, 2009 Tom Tromey <tromey@redhat.com>

;; Author: Tom Tromey <tromey@redhat.com>
;; Created: 10 Mar 2007
;; Version: 2.0pre
;; Keywords: tools

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To use this, put package.el somewhere on your load-path.  Then add
;; this to your .emacs:
;;
;;    (load "package")
;;    (package-initialize)
;;
;; This will automatically make available the packages you have
;; installed using package.el.  If your .emacs will refer to these
;; packages, you may want to initialize the package manager near the
;; top.
;;
;; Note that if you want to be able to automatically download and
;; install packages from ELPA (the Emacs Lisp Package Archive), then
;; you will need the 'url' package.  This comes with Emacs 22; Emacs
;; 21 users will have to find it elsewhere.
;;
;; If you installed package.el via the auto-installer:
;;
;;    http://tromey.com/elpa/
;;
;; then you do not need to edit your .emacs, as the installer will
;; have done this for you.  The installer will also install the url
;; package if you need it.

;; Other external functions you may want to use:
;;
;; M-x package-list-packages
;;    Enters a mode similar to buffer-menu which lets you manage packages. You
;;    can choose packages for install (mark with "i", then "x" to execute) or
;;    deletion, and you can see what packages are available. This will
;;    automatically fetch the latest list of packages from your selected
;;    archives.
;;
;; M-x package-list-packages-no-fetch
;;    Like `package-list-packages', but does not automatically fetch the new list
;;    of packages.
;;
;; M-x package-install-from-buffer
;;    Install a package that appears in the current buffer. The package must
;;    either be a single .el file or a tar archvie in the format described below
;;    in "Tar Package Format." For single .el files, this only works for
;;    packages which define a Version header properly; package.el also supports
;;    the extension headers Package-Version (in case Version is an RCS id or
;;    similar), and Package-Requires (if the package requires other packages).
;;
;; M-x package-install-file
;;    Install a package from the indicated file. The package can be either a tar
;;    file or a .el file. A tar file must contain an appropriately-named ".epkg"
;;    file; a .el file must be properly formatted as with
;;    `package-install-from-buffer'.

;; The idea behind package.el is to be able to download packages and
;; install them.  Packages are versioned and have versioned
;; dependencies.  Furthermore, this supports built-in packages which
;; may or may not be newer than user-specified packages.  This makes
;; it possible to upgrade Emacs and automatically disable packages
;; which have moved from external to core.  (Note though that we don't
;; currently register any of these, so this feature does not actually
;; work.)

;; This code supports a single package repository, ELPA.  All packages
;; must be registered there.

;; A package is described by its name and version.  The distribution
;; format is either  a tar file or a single .el file. See "Tar Package Format"
;; below for more details on the tar format.

;; A .el file will be named "NAME-VERSION.el" in ELPA, but will be
;; installed as simply "NAME.el" in a directory named "NAME-VERSION".

;; The downloader will download all dependent packages.  It will also
;; byte-compile the package's Lisp at install time.

;; At activation time we will set up the load-path and the info path,
;; and we will load the package's autoloads.  If a package's
;; dependencies are not available, we will not activate that package.

;; Conceptually a package has multiple state transitions:
;;
;; * Download.  Fetching the package from ELPA.
;; * Install.  Untar the package, or write the .el file, into
;;   ~/.emacs.d/elpa/ directory.
;; * Byte compile.  Currently this phase is done during install,
;;   but we may change this.
;; * Activate.  Evaluate the autoloads for the package to make it
;;   available to the user.
;; * Load.  Actually load the package and run some code from it.

;;; Tar Package Format:

;; A tar file must be named "NAME-VERSION.tar". The tar file must unpack into
;; a directory named after the package and version: "NAME-VERSION". All of the
;; package files must be in this directory, and it must include a file
;; "info.epkg" in the unpacked directory which contains the package metadata
;; (see "Package Metadata" below).

;;; Package Metadata:
;;
;; The `package' structure is used throughout this library and contains metadata
;; about an individual version of a package. It contains the following fields
;; (most of which are inherited from `elx-pkg', which see):
;;
;; * name
;; * version
;; * version-raw
;; * summary
;; * created
;; * updated
;; * license
;; * authors
;; * maintainer
;; * provides
;; * requires-hard
;; * requires-soft
;; * keywords
;; * homepage
;; * wikipage
;; * commentary
;; * archive
;; * type
;;
;; See the documentation of the `package' and `elx-pkg' structures for more
;; information about each of the fields.
;;
;; When saved to disk, a package is written as a plist, with each of the fields
;; as a property and that field's value as the plist value. This means that an
;; epkg file would look like:
;;
;;     (:name 'package
;;      :version '(0 9 5)
;;      :version-raw "0.9.5"
;;      ...
;;      )
;;
;; and so on, for each field defined.


;;; Package Archive Format:
;;
;; Each archive has its own path, under which the archive metadata and installed
;; packages live. By default, the main "ELPA" archive lives in the
;; "~/.emacs.d/elpa/" directory. Within the archive directory is an
;; "archive-contents" file which contains a description of all of the packages
;; provided by that archive. This file is of the form
;;
;;     (2
;;       PACKAGE...
;;     )
;;
;; where PACKAGE is a plist package description as described above in "Package
;; Metadata". 2 is the version number `package-archive-version', which describes
;; the archive format version.
;;
;; Additionally, there is a directory for each installed package from that
;; archive whose name is "PACKAGE-VERSION". This directory contains the contents
;; of the package, such as the Emacs Lisp files and any resources (such as
;; images) used by that package. Additionally, the metadata file "info.epkg" and
;; an autoload file named "autoloads.el" are included in the directory.
;; "info.epkg" contains information about the package in the format described in
;; "Package Metadata". The "autoloads.el" file contains the
;; automatically-extracted autoload information from the package.

;; This means that if the package "bm" version "1.37" was installed from the
;; "elpa" archive which has the base directory "~/.emacs.d/elpa", then the
;; following files would exist:
;;
;; * ~/emacs.d/elpa/bm-1.37/bm.el
;; * ~/emacs.d/elpa/bm-1.37/autoloads.el
;; * ~/emacs.d/elpa/bm-1.37/info.epkg

;;; Thanks:
;;; (sorted by sort-lines):

;; Jim Blandy <jimb@red-bean.com>
;; Karl Fogel <kfogel@red-bean.com>
;; Kevin Ryde <user42@zip.com.au>
;; Lawrence Mitchell
;; Michael Olson <mwolson@member.fsf.org>
;; Sebastian Tennant <sebyte@smolny.plus.com>
;; Stefan Monnier <monnier@iro.umontreal.ca>
;; Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Phil Hagelberg <phil@hagelb.org>
;; Samuel Bronson <naesten@gmail.com>

;;; History:
;;
;; Originally written by Tom Tromey, multiple archive support added by Phil
;; Hagelberg.

;;; Code:

(require 'assoc)
(require 'elm)
(require 'autoload)
(require 'tar-mode)
(require 'dired)
(eval-when-compile (require 'cl))

(defvar package-user-dir
  (expand-file-name (convert-standard-filename "~/.emacs.d/elpa/"))
  "Name of the directory where the user's packages are stored.")

(defcustom package-archives `((elpa "http://tromey.com/elpa/" ,(concat package-user-dir "elpa"))
                              (manual nil ,(concat package-user-dir "manual"))
                              (builtin nil "/usr/share/emacs/site-lisp/elpa/"))
  "An alist of archives (names, URLs, and local paths) from which to fetch.

The archive name must be a symbol, while the repository URL is a
string.

The default points to ELPA, the Emacs Lisp Package Archive."
  :type '(alist :key-type (symbol :tag "Archive name")
                :value-type (group (choice :tag "Archive URL"
                                           (string :tag "URL")
                                           (const :tag "None" nil))
                                   (string :tag "Local path")))
  :group 'package
  :package-version '("package.el" . "0.9.3"))

(defconst package-archive-versions '(2 1)
  "A list of archive version numbers understood by this library.

Versions earlier in the list are preferred to those appearing
later.")

(defconst package-info-filename "info.epkg"
  "The name of the package metadata file in each package directory.

Each package directory will contain a file with this name which
contains the metadata about the package. This can be loaded as a
`package' structure.")

(defconst package-archive-contents-filename "archive-contents"
  "The name of the file which contains the package archive contents.

Each archive has a local file which contains the list of packages
provided by that archive. This constant is the name of that file
within an archive's local path.")

(defconst package-version "0.9.5pre"
  "Version of package.el.")

(defconst package-types '((single . "el")
                          (tar . "tar")
                          (builtin . nil))
  "Allowed file types for package files.

A list of (TYPE-NAME . EXTENSION). The special type \"builtin\"
doesn't have an extension, since it will never be independently
downloaded or dealt with in any way aside from resolving
dependencies.")

(defconst package-statuses '((available . "avail")
                             (installed . "inst")
                             (activated . "act")
                             (obsolete  . "obs"))
  "Possible statuses of a package with abbreviations.

At any time, a package is in exactly one of these states. This is
an alist of the form (STATUS . ABBREV), where STATUS is the
package status (as a symbol) and ABBREV is the string
abbreviation of the status. The mean of each status is described
below:

 - `available': The package exists within an archive and is
   available to be downloaded and installed.

 - `installed': The package files exist on the local computer,
   but the package has not been activated. Activating a package
   primarily involves loading that package's \"autoloads\" file
   and adding its directory to `load-path'.

 - `activated': The package has been installed and activated, its
   autoloads registered, and its install directory added to
   `load-path'.

 - `obsolete': This package has been superseded by a newer
   version, and can be safely uninstalled. Packages are only
   marked obsolete when the newer version is `installed' or
   `activated'.")

(defconst package-status-default 'available
  "Status to assign to packages which don't provide their own.")

(defstruct (package (:include elx-pkg)
                    (:constructor inherit-package
                                  (pkg
                                   &key archive type status
                                   &aux (name (elx-pkg-name pkg))
                                   (version (elx-pkg-version pkg))
                                   (version-raw (elx-pkg-version-raw pkg))
                                   (summary (elx-pkg-summary pkg))
                                   (created (elx-pkg-created pkg))
                                   (updated (elx-pkg-updated pkg))
                                   (license (elx-pkg-license pkg))
                                   (authors (elx-pkg-authors pkg))
                                   (maintainer (elx-pkg-maintainer pkg))
                                   (provides (elx-pkg-provides pkg))
                                   (requires-hard (elx-pkg-requires-hard pkg))
                                   (requires-soft (elx-pkg-requires-soft pkg))
                                   (keywords (elx-pkg-keywords pkg))
                                   (homepage (elx-pkg-homepage pkg))
                                   (wikipage (elx-pkg-wikipage pkg))
                                   (commentary (elx-pkg-commentary pkg)))))
  "Extends the `elx-pkg' structure with archive-specific information.

This contains the complete info about a package as contained in
the archive index. The fields are:

 - ARCHIVE: The archive from which this package comes, as a symbol.

 - TYPE: The distribution type of the package, must one of the
   types in `package-types'.

 - STATUS: The installed status of this package, see
   `package-statuses'.

The special constructor, `inherit-package' allows constructing a
`package' struct from an existing `elx-pkg' struct. Extra
arguments are supported by keys."
  archive
  type
  status)

(defun package-property-get (pkg prop)
  "Returns the PROP property of PKG"
  (let ((func (intern (format "package-%s" prop))))
    (funcall func pkg)))

(defsubst package-version-canonical (pkg)
  "Return the canonical version of PKG.

Simply passes it through `elx-version-canonical'."
  (elx-version-canonical (package-version pkg)))

(defsubst package-status-string (pkg)
  "Return the string representation of the status of PKG."
  (cdr (assoc (package-status pkg) package-statuses)))

(defun package-status-symbol (str)
  "Return the package status symbol corresponding to STR.

More or less the reverse of `package-status-string'."
  (car (rassoc str package-statuses)))

(defun package-suffix (pkg &optional noerror)
  "Gets the download suffix for PKG.

If PKG is a builtin package, signals an error unless NOERROR is
non-nil."
  (let* ((type (package-type pkg))
         (suffix (aget package-types type t)))
    (if (and (eq type 'builtin) (not noerror))
        (error "Package is a builtin, and therefore does not have a suffix")
      suffix)))

(defun package-type-from-filename (file &optional noerror)
  "Return the package type of FILE based on its name.

Examines `package-types' for a suffix which matches that of FILE.
If no match is found, signals an error unless NOERROR is
non-nil, in which case nil is returned."
  (let* ((ext (file-name-extension file))
         (result (car (find ext package-types
                            :key 'cdr
                            :test 'equal))))
    (unless (or result noerror)
      (error "Could not find package type for extension: %s" ext))
    result))

(defvar package-registry
  nil
  "Alist of all packages available for installation.

This is an alist of the form (NAME . (PACKAGE...)), where NAME is
the symbol name of a package and PACKAGE is an individual
`package' structure.

More than one package is allowed for each name, since there may
be multiple versions of a package available or two archives
may each have different versions of a package available.")

(defun package-registry-flat ()
  "Returns a flattened version of `package-registry'.

Sometimes, it is useful to have a flattened list of packages, so
this function provides that."
  (loop for (name . pkgs) in package-registry
        append pkgs))

(defun* package-find (basic-name &key
                           (name basic-name)
                           version
                           version-raw
                           summary
                           created
                           updated
                           license
                           authors
                           maintainer
                           provides
                           requires-hard
                           requires-soft
                           keywords
                           homepage
                           wikipage
                           commentary
                           archive
                           type
                           status)
  "Search `package-registry' for a package named NAME.

Returns a list of matches, since there may be more than one
package with the same name (i.e. different versions).

The optional keyword arguments allow the results to be narrowed
down to return only those packages which match all of the
supplied keywords. For example:

    (package-find 'package :version '(0 9 5))

Would return a list of packages called 'package with version
number \"0.9.5\", if any exist."
  (let ((pkgs (aget package-registry name)))
    (dolist (slot
             ;; This is `cddr' to skip the `name' slot, as well as the cl-tag.

             ;; TODO: Avoid computing slots on every invocation; it (probably)
             ;; won't change in successive runs
             (cddr (mapcar 'car (get 'package 'cl-struct-slots)))
             pkgs)
      (when (symbol-value slot)
        (setq pkgs (remove* (symbol-value slot) pkgs
                            :test-not 'equal
                            :key (intern (concat "package-" (symbol-name slot)))))))))

;; TODO: Resolve multiple matches using archive priority?
(defun* package-find-latest (name &optional noerror &rest keys)
  "Find the newest version of package NAME.

If NOERROR is nil, signal an error when no matching package is
found, otherwise return nil.

KEYS is a set of keyword arguments to be passed to
`package-find'. If the :version keyword is present, it is
considered an error, and an error is signaled unless NOERROR is
non-nil.

Uses `package-find' to search for packages named NAME matching
KEYS and returns the one with the greatest version number.

If there are multiple packages with the same name and version,
only one is returned; there is no guarantee of which one that
will be."
  ;; Ignore the :version keyword; that is the entire point of this function.
  (when (plist-get keys :version)
    (noreturn (if noerror
         (noreturn (return-from package-find-latest nil))
       (error "Version already specified; how do you expect me to find the latest?"))))

  (let* ((pkgs (apply 'package-find name keys))
         (result (car-safe pkgs)))
    (dolist (pkg (cdr-safe pkgs))
      (when (version-list-< (package-version result) (package-version pkg))
        (setq result pkg)))
    (if (or result noerror)
        result
      (error "No package found named '%s' matching parameters '%s'" name keys))))

(defun package-find-rest (pkg &optional noerror)
  "Fill in the missing information of PKG from `package-registry'.

Searches for a package in `package-registry' which matches all of
the non-nil slots of PKG and adds any information from the found
package to PKG.

If more than one matching package is found, an error is signaled
unless NOERROR is non-nil.

This is used when there is a partial package (say, parsed from a
menu line), and you want to fill in the rest of the information."
  (let* ((key-list (cl-merge-mapslots '(lambda (slot accessor val)
                                         (if val
                                             (list (intern (format ":%s" slot)) val)
                                           nil))
                                      'package pkg))
         (keys (loop for elt in (delq nil key-list)
                     append elt))
         (found (apply 'package-find (package-name pkg) keys)))
    (if (eq (length found) 1)
        (cl-merge-struct 'package pkg (car found))
      (if noerror
          nil
        (error "Expected only a single matching package, %d found" (length found))))))

(defun package-archive-url (archive &optional noerror)
  "Returns the Url containing information about ARCHIVE.

ARCHIVE must be the symbol name of an archive. If ARCHIVE is
'builtin, then an error is signaled unless NOERROR is non-nil.
The built-in packages cannot be downloaded using package.el, so
an archive URL is meaningless for them.

Each archive in `package-archives' is checked."
  (when (eq archive 'builtin)
    (unless noerror
      (error "Builtin archive does not have a download URL")))
  (nth 0 (aget package-archives archive)))

(defun package-archive-localpath (archive)
  "Returns the local path of ARCHIVE.

ARCHIVE must be the symbol name of an archive.

Each archive in `package-archives' is checked."
  (expand-file-name (file-name-as-directory (nth 1 (aget package-archives archive)))))

(defun package-archive-content-file (archive)
  "Returns the path of the content file of ARCHIVE.

ARCHIVE must be the symbol name of an archive."
  (concat (package-archive-localpath archive) package-archive-contents-filename))

(defun package-read-file (file &optional noerror)
  "Read `package' data.

FILE is the file to read. Returns a `package' structure if
successful. Signals an error if FILE cannot be read unless
NOERROR is non-nil."
  (if (and (file-readable-p file)
           (file-regular-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (package-read-string (buffer-string)))
    (if noerror
        nil
      (error "File %s not readable" file))))

(defun package-read-string (str &optional noerror)
  "Read `package' structure data from STR.

Signals an error if something goes wrong unless NOERROR is
non-nil."
  (condition-case err
      (apply 'make-package (package-read-from-string str))
    (error (if noerror
               nil
             (signal (car err) (cdr err))))))

(defun package-register (pkg)
  "Register package PKG if it isn't already in `package-registry'.

Returns nil if PKG was already in the list or PKG if it was not."
  (let* ((pkg-name (package-name pkg))
        (existing-pkgs (aget package-registry pkg-name)))
    (if existing-pkgs
      (unless (member pkg existing-pkgs)
        (nconc existing-pkgs (list pkg))
        pkg)
      (aput 'package-registry pkg-name (list pkg)))))

(defsubst package-load-descriptor (pkg)
  "Return information the info file of PKG.

PKG can be a minimal `package' structure; only
the :name, :version, and :archive fields are needed.

Return nil if the package could not be found."
  (package-read-file (package-info-file pkg)))

(defun package-split-filename (file &optional suffix noerror)
  "Split FILE into a name and version.

FILE must be a directory of the form \"NAME-VERSION\" which will
be split into a cons cell with the form (NAME . VERSION), where
NAME is an interned symbol and VERSION is a list as returned by
`version-to-list'. FILE can be either a relative or absolute
filename, only the last element of the filename (which should be
the directory to examine) will be considered.

If optional argument SUFFIX is provided, strip the suffix from
FILE before processing the name.

If NOERROR is non-nil, returns nil rather than signaling an error
when FILE cannot be resolved to a name and version."
  (when suffix
    (setq file (replace-regexp-in-string
                (format "\\.%s$" suffix)
                ""
                file)))
  (let* ((local-dir (file-name-nondirectory (directory-file-name file)))
         (parts (split-string local-dir "-" t))
         (version (car (last parts)))
         (name (combine-and-quote-strings (butlast parts 1) "-")))
    (condition-case err
        ;; TODO: Add some format checks to `name'.
        (cons (intern name)
              (version-to-list version))
      (error (if noerror
                 nil
               (signal (car err) (cdr err)))))))

(defun package-from-filename (file &optional suffix noerror)
  "Create a skeleton `package' structure from FILE.

This is mainly used to create a package with enough information
that `package-info-file' can find the info file for the package.

Optional argument SUFFIX is a string to strip from the end of
FILE. If a package cannot be created, an error is signaled unless
NOERROR is non-nil, in which case nil is returned.

Searches `package-archives' for a prefix which contains FILE and
then uses the tail directory to determine the package name and
version."
  (when suffix
    (setq file (replace-regexp-in-string
                (format "\\.%s$" suffix)
                ""
                file)))
  (let* ((type (package-type-from-filename file noerror))
         (local-file (file-name-nondirectory (directory-file-name file)))
         (file-info (package-split-filename file
                                            (aget package-types type t)
                                            noerror))
         archive)
    (loop for (arch info) in package-archives
          for arch-path = (package-archive-localpath arch)
          for path-len = (length arch-path)
          if (equal (substring file 0 path-len) arch-path)
          do (setq archive arch))
    (if (or archive noerror)
        (make-package :name (car file-info)
                      :version (cdr file-info)
                      :type type
                      :archive archive)
      (error "Could not find an archive containing file: %s" file))))

;; TODO: Add special handling of builtin packages, so that directories don't
;; need to be created for each builtin package.
(defun package-register-all-installed ()
  "Register metadata of all installed packages.

Uses `package-archives' to find packages."
  (loop for (archive ign) in package-archives
        for archive-dir = (package-archive-localpath archive)
        when (and (file-readable-p archive-dir)
                  (file-directory-p archive-dir))
        do (loop for pkg-dirname in (directory-files archive-dir t "^[^.]")
                 for pkg = (package-load-descriptor
                            (package-from-filename pkg-dirname))
                 do (setf (package-status pkg) 'installed)
                 do (package-register pkg))))

(defun package-install-directory (pkg &optional relative)
  "Return the install directory for PKG.

The install directory is where a particular package is (or would
be, for un-installed packages) installed. Packages are installed
within a sub-folder of their archive's local path named
\"NAME-VERSION\", where NAME is the name of the package and
VERSION is the version of the package after being processed by
`package-version-canonical'.

If RELATIVE is non-nil, return the install directory of PKG
relative to its archive root."
  (let* ((name (symbol-name (package-name pkg)))
         (version (package-version-canonical pkg))
         (archive-dir (if (eq name 'package)
                          ;; The package for package.el is handled specially.
                          package-user-dir
                        (package-archive-localpath (package-archive pkg))))
         (raw-name (format "%s/%s-%s" archive-dir name version))
         (abs-name (convert-standard-filename
                    (file-name-as-directory (expand-file-name
                                             raw-name)))))
    (if relative
        (file-relative-name abs-name archive-dir)
      abs-name)))

(defun package-install-file-path (pkg)
  "Returns the install file for PKG.

PKG must have type `single', since there is not a single install
file for either `tar' or `builtin' packages."
  (unless (eq (package-type pkg) 'single)
    (error "Package type must be `single' to get an install file; given: %s" (package-type pkg)))
  (concat (package-install-directory pkg)
          (symbol-name (package-name pkg))
          "."
          (package-suffix pkg)))

(defsubst package-info-file (pkg &optional relative)
  "Returns the info (.epkg) file for PKG.

If RELATIVE is non-nil, return the path of the info file relative
to the archive directory of PKG."
  (concat (package-install-directory pkg relative) package-info-filename))

(defun package-download-url (pkg)
  "Return the download URL of PKG.

The download URL is the URL from which PKG can be downloaded.
This depends on the base URL of the package's archive."
  (format "%s%s-%s.%s" (package-archive-url (package-archive pkg))
          (package-name pkg)
          (package-version-canonical pkg)
          (package-suffix pkg)))

(defun package-autoload-file (pkg)
  "Return the full path of the autoload file for PKG."
  (concat (package-install-directory pkg) "autoloads.el"))

;; TODO: Re-add info handling, used to add `package-install-directory' to
;; `Info-directory-list'.
(defun package-do-activate (pkg)
  "Set up a single PKG after it has been installed.

Modifies `load-path' to include the package directory and loads
the `autoload' file for the package."
  (add-to-list 'load-path (package-install-directory pkg))
  (load (package-autoload-file pkg) nil t)
  (setf (package-status pkg) 'activated))

;; FIXME: return a reason instead?
(defun package-activate (pkg)
  "Try to activate PKG.

Signal an error if the package could not be activated.

Recursively activates all dependencies of PKG."
  ;; Assume the user knows what he is doing -- go ahead and activate a
  ;; newer version of a package if an older one has already been
  ;; activated.  This is not ideal; we'd at least need to check to see
  ;; if the package has actually been loaded, and not merely
  ;; activated.
  (let ((name (package-name pkg)))
    (cond
     ((eq (package-status pkg) 'activated))
     ;; Don't try to activate 'emacs', that's just silly.
     ((eq name 'emacs))
     ;; If this package is already the most recently installed version, no
     ;; further action is needed.
     ((equal pkg (package-find-latest name t :status 'activated)))
     ((member pkg (package-find name)))
     (t
      ;; Signal an error if a hard requirement cannot be found, but not for a
      ;; soft requirement.
      (dolist (req (package-required-packages pkg 'hard))
        (package-activate (package-find-latest req nil)))
      (dolist (req (package-required-packages pkg 'soft))
        (package-activate (package-find-latest req t)))
      (package-do-activate pkg)))))

(defun package-generate-autoloads (pkg)
  "Generate autoload definitions for PKG."
  (let ((generated-autoload-file (package-autoload-file pkg))
        (version-control 'never))
    (update-directory-autoloads pkg-dir)))

(defsubst package-parent-directory (dir)
  "Return the parent directory of DIR.

This is pretty primitive, and should really be included in Emacs."
  (file-name-directory (directory-file-name dir)))

(defun package-untar-buffer (&optional buf dir)
  "Untar BUF or the current buffer to DIR.

If BUF is nil, then use the current buffer. If DIR is nil, use
the current value of `default-directory'.

This uses `tar-untar-buffer' if it is available. Otherwise it
uses an external `tar' program."
  (unless buf
    (setq buf (current-buffer)))
  (unless dir
    (setq dir default-directory))

  (with-current-buffer buf
    (let ((default-directory dir))
      (if (fboundp 'tar-untar-buffer)
          (progn
            ;; tar-mode messes with narrowing, so we just let it have the
            ;; whole buffer to play with.
            (delete-region (point-min) (point))
            (tar-mode)
            (tar-untar-buffer))
        ;; FIXME: check the result.
        (call-process-region (point) (point-max) "tar" nil nil nil
                             "xf" "-")))))

(defun package-unpack-tar (pkg buf)
  "Unpack a tar PKG from BUF.

BUF is expected to contain a tarred package archive. If BUF is
nil, the current buffer is used."
  (let ((pkg-dir (package-install-directory pkg)))
    ;; (if (file-directory-p pkg-dir)
    ;;     (mapc (lambda (file) nil) ;; TODO: 'delete-file when we're more confident
    ;;           (directory-files pkg-dir t "^[^.]")))
    (package-untar-buffer buf (package-parent-directory pkg-dir))))

(defun package-unpack-single (pkg buf)
  "Extract and install PKG from contents of BUF.

PKG is the package metadata and BUF is the buffer from which to
install the package."
  (let ((pkg-file (package-install-file-path pkg)))
    (when (and (not (eq (package-type pkg) 'package))
               (file-exists-p pkg-file))
      (error "Destination file %s exists, refusing to overwrite" pkg-file))
    (with-temp-file pkg-file
      (with-current-buffer buf
        (buffer-substring)))))

(defun package-unpack (pkg &optional buf)
  "Unpack and install PKG from BUF or the current buffer.

Uses `package-type' to determine which function should be used to
install PKG."
  (let ((buf (or buf (current-buffer)))
        (pkg-info-file (package-info-file pkg))
        (pkg-dir (package-install-directory pkg)))
    (make-directory pkg-dir t)
    (apply (intern (format "packge-unpack-%s" (package-type pkg)))
           (list pkg buf))
    (unless (file-exists-p pkg-info-file)
      (with-temp-file pkg-info-file
        (insert (cl-merge-pp pkg))))
    (package-generate-autoloads pkg)
    (let ((load-path (append (list pkg-dir) load-path)))
      (byte-recompile-directory pkg-dir 0 t))))

(defun package-handle-response (&optional buf)
  "Handle the response from the server.

Parse the response and signal an error if the download failed.
The url package seems to require extra processing for this.
Either BUF or the current buffer is used as the response buffer
from `url-retrieve-synchronously'.

It will remove any headers and move the point to the beginning of
the buffer."
  (let ((type (url-type url-current-object))
        (buf (or buf (current-buffer))))
    (with-current-buffer buf
      (cond
       ((equal type "http")
        (let ((response (url-http-parse-response))
              header-end)
          (unless (eq (/ response 100) 2)
            (display-buffer (current-buffer))
            (error "Error during download request:%s"
                   (buffer-substring-no-properties (point) (progn
                                                             (end-of-line)
                                                             (point)))))
          ;; Strip HTTP headers
          (ietf-drums-narrow-to-header)
          (setq header-end (1+ (point-max)))
          (widen)
          (delete-region (point-min) header-end)
          (goto-char (point-min))))
       ((equal type "file")
        nil)))))

(defun package-download (pkg)
  "Download and install PKG.

The particular handler is determined by the :type attribute of
PKG.

Builtin packages cannot be downloaded (since they are already
built in) and so signal an error if PKG has :type 'builtin."
  (when (eq (package-type pkg) 'builtin)
    (error "Attempted to download builtin package %s" (package-name pkg)))
  (let ((buf (url-retrieve-synchronously
              (package-download-url pkg))))
    (package-handle-response buf)
    (package-unpack pkg buf)
    (kill-buffer buf)))

(defun package-required-packages (pkg &optional type)
  "Return a list of the required packages of PKG.

Unlike `elx-required-packages', this returns a list of the
package structures, and not the features within those packages
that are required.

TYPE determines which kind of required packages are returned:
hard, soft, or both. Its behavior is as follows:

 * nil or 'both: Return a merged list of both hard and soft
   requirements.

 * 'hard: Return a list of only the hard requirements.

 * 'soft: Return a list of only the soft requirements."
  (let ((hard (loop for (name . provides) in (package-requires-hard pkg)
                    collect (package-find-latest
                             name nil :provides provides)))
        (soft (loop for (name . provides) in (package-requires-soft pkg)
                    collect (package-find-latest
                             name nil :provides provides))))
    (cond
     ((or (null type) (eq type 'both))
      (append hard soft))
     ((eq type 'hard)
      hard)
     ((eq type 'soft)
      soft)
     (t
      (error "TYPE must be 'hard, 'soft, 'both, or nil; '%s' received" type)))))

(defun package-compute-transaction (result requirements)
  "Recursively prepare a transaction, resolving dependencies.

RESULT is a flattened list of packages to install.
`package-compute-transaction' recursively builds this argument
before passing it up to the caller.

REQUIREMENTS is a list of required packages, to be recursively
processed to resolve all dependencies (if possible)."
  (loop for req in requirements
        unless (member req result) append (package-compute-transaction
                                           (append (list req) result)
                                           (package-required-packages req))
        into temp
        finally return (remove-duplicates (append temp result)
                                          :from-end t)))

(defun package-read-from-string (str)
  "Read a Lisp expression from STR.

Signal an error if the entire string was not used."
  (let* ((read-data (read-from-string str))
         (more-left
          (condition-case nil
              ;; The call to `ignore' suppresses a compiler warning.
              (progn (ignore (read-from-string
                              (substring str (cdr read-data))))
                     t)
            (end-of-file nil))))
    (if more-left
        (error "Can't read whole string")
      (car read-data))))

;; TODO: Attempt to download README into :commentary slot?
(defun package-from-version-1 (old-spec)
  "Reads a version 1 package from OLD-SPEC into a v2 package."
  (let ((name (car old-spec))
        (vec (cdr old-spec)))
      (make-package :name name
                    :version (aref vec 0)
                    :requires-hard (loop for (name version) in (aref vec 1)
                                         collect (list (cons name version)))
                    :summary (aref vec 2)
                    :type (aref vec 3))))

(defun package-read-archive-contents (source)
  "Read the contents for SOURCE.

Will return the data from the file, or nil if the file does not
exist. Will signal an error if the archive version is not
supported.

SOURCE is passed directly to `read', so should be a valid input
for that function."
  (let* ((contents (read source))
         (arch-version (car contents)))
    (unless (memq arch-version package-archive-versions)
      (error "Package archive version %d is not one of %s"
             arch-version package-archive-versions))
    (cdr contents)))

(defun package-register-all-archive-contents ()
  "Read the archive description of each of the archives in `package-archives'."
  (loop for (archive . info) in package-archives
        do (package-register-archive archive)))

(defun package-register-archive (archive)
  "Re-read `archive-contents' for ARCHIVE.

Will add any new packages found `package-registry'. Will signal
an error if the archive version is too new or if a
package's :archive field does not match ARCHIVE."
  (let* ((buf (find-file-noselect (package-archive-content-file archive)))
         (archive-contents (package-read-archive-contents buf)))
    (kill-buffer buf)
    (if archive-contents
        (dolist (pkg archive-contents)
          ;; TODO: Use a hook for validating packages before they hit the registry?
          (unless (eq (package-archive pkg) archive)
            (error "Package %s lists %s as its archive, but was read from archive %s"
                   (package-name pkg)
                   (package-archive pkg)
                   archive))
          (package-register pkg)))))

(defun package-download-transaction (transaction)
  "Download and install all the packages in the given TRANSACTION."
  (loop for pkg in transaction
        do (package-download pkg)))

(defun package-install (pkg)
  "Install the package PKG.

Interactively, prompts for the package name."
  (interactive
   (list (package-find-latest (intern (completing-read "Install package: "
                                   (loop for (name . pkgs) in package-registry
                                         collect (symbol-name name))
                                   nil t))
                              nil)))
  ;; TODO: Don't loop twice through package list. Make a `package-find' variant
  ;; which takes a package and looks for a total match in `package-registry'.
  (let* ((name (package-name pkg))
         (version (or (package-version pkg)
                      (package-version (package-find-latest name))))
         (pkg (car (package-find name :version version))))
    (unless pkg
      (error "Package '%s', version '%s' not available for installation"
             name version))
    ;; TODO: Make a function which combines these two steps.
    (let ((transaction
           (package-compute-transaction (list pkg)
                                        (package-requires-hard pkg))))
      (package-download-transaction transaction)))
  ;; Try to activate it.
  (package-initialize))

(defun package-from-single-buffer (buf)
  "Create a package structure from BUF.

BUF must be an Emacs Lisp source code file which is parseable by
`elx-package-metadata'."
  (inherit-package (elx-package-metadata buf)
                   :archive 'manual
                   :type 'single))

(defun package-type-from-buffer (buf)
  "Determine the package type from the contents of BUF."
  (with-current-buffer buf
    (condition-case err
        (progn
          (tar-mode)
          'tar)
      (error (emacs-lisp-mode)
             'single))))

(defun package-tar-items (buf)
  "Walk through BUF (as a tar buffer) and return the items.

Return each of the header structures parsed from BUF."
  (with-current-buffer buf
    (when (tar-data-swapped-p)
      (tar-swap-data))
    (set-buffer-multibyte nil)
    (goto-char (point-min))
    (loop
     for pos = (point-min) then (tar-header-data-end hdr)
     for hdr = (tar-header-block-tokenize pos tar-file-name-coding-system)
     while (and (< pos (point-max)) hdr)
     for link-type = (tar-header-link-type hdr)
     for name = (tar-header-name hdr)
     for size = (tar-header-size hdr)
     collect hdr)))

(defun package-tar-item-contents (hdr)
  "Extract the contents from a single tar header HDR.

The tar buffer from which HDR was parsed must still be active,
since this function reads data from that buffer."
  (let* ((start (tar-header-data-start hdr))
         (end (+ start (tar-header-size hdr)))
         (buf (marker-buffer start)))
    (with-current-buffer buf
      (buffer-substring-no-properties start end))))

(defun package-from-tar-buffer (buf &optional noerror)
  "Find package information for a tar file in BUF.

BUF is a buffer containing raw tar data. If there is a problem,
then an error is signaled unless NOERROR is non-nil."
  (let* ((items (package-tar-items buf))
         (dir-hdr (find-if '(lambda (item)
                              (and (package-split-filename (car item) nil t)
                                   ;; 5 is the tar link-type for a directory
                                   (eq (cdr item) 5)))
                           items
                           :key '(lambda (item) (cons (tar-header-name item)
                                                      (tar-header-link-type item)))))
         ;; Check that we actually received successfully found dir-hdr
         (name-vers (package-split-filename (tar-header-name dir-hdr)))
         (pkg-skel (make-package :name (car name-vers)
                                 :version (cdr name-vers)
                                 ;; Doesn't actually matter, since we will be
                                 ;; returning only the relative path.
                                 :archive 'manual))
         (info-file (package-info-file pkg-skel t))
         (pkg-hdr (find info-file items
                        :key 'tar-header-name
                        :test 'equal))
         (start (tar-header-data-start pkg-hdr))
         (end (+ start (tar-header-size pkg-hdr)))
         (pkg-data (with-current-buffer buf
                     (buffer-substring start end))))
    (package-read-string pkg-data)))

(defun package-from-buffer (buf &optional noerror)
  "Generate and return a package structure from BUF.

Signals an error on failure unless NOERROR is non-nil."
  (let* ((type (package-type-from-buffer buf))
         (func (intern (format "package-from-%s-buffer" type))))
    (funcall func buf noerror)))

;; Merge this somehow with (a less-ugly) `package-from-tar-buffer'.
(defun package-from-tar-file (file)
  "Find package information for a tar file.

FILE is the path to a tar archive."
  (let* ((pkg (package-from-filename file))

         (pkg-info (shell-command-to-string
                    ;; Requires GNU tar.
                    (concat "tar -xOf " file " "
                            (package-info-file pkg t))))
         (pkg-new (apply 'make-package (package-read-from-string pkg-info))))

    ;; TODO: Maybe add some more sanity checks... Use a hook to avoid having to
    ;; hard-code in the checks to this function.
    (unless (eq (package-name pkg-new) (package-name pkg))
      (error "Inconsistent package names"))
    (unless (equal (package-version pkg-new) (package-version pkg))
      (error "Inconsistent package versions"))
    (unless (equal (package-archive pkg-new) (package-archive pkg))
      (error "Inconsistent package archives"))
    pkg-new))

(defsubst package-from-single-file (file)
  "Returns a package structure for FILE."
  (with-temp-buffer file
                    (insert-file-literally file)
                    (package-from-single-buffer (current-buffer))))

(defun package-from-file (&optional source)
  "Return a package structure from SOURCE.

SOURCE is either a buffer, a file, or nil, meaning use the
current buffer."
  (unless source
    (setq source (current-buffer)))
  (cond
   ((and (stringp source) (file-readable-p source))
    (let* ((type (package-type-from-filename source))
           (extractor (intern (format "package-from-%s-file" type))))
      (funcall extractor source)))
   ((buffer-live-p (get-buffer source))
    (case (package-type-from-buffer source)
      ('single (package-from-single-buffer source))
      ('tar (package-from-tar-buffer source))
      (t (error "Only 'single and 'tar packages can be processed"))))))

(defun package-install-from-buffer (buf &optional pkg)
  "Install a package from BUF.

If PKG is provided, use that as the package metadata. Otherwise
attempt to read it from BUF."
  (interactive "bInstall package from buffer:")
  (setq pkg (if (package-p pkg)
                (cl-merge-struct 'package pkg (package-from-buffer buf))
              (package-from-buffer buf)))

  (package-download-transaction
   (package-compute-transaction nil (package-required-packages pkg)))
  (package-unpack pkg buf)
  ;; Try to activate it.
  (package-initialize))

(defun package-install-from-file (file)
  "Install a package from FILE.

The file must match one of the extensions in `package-types'."
  (interactive "fPackage file name: ")
  (let ((pkg (package-from-file file)))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (package-install-from-buffer (current-buffer) pkg))))

(defun package-delete (pkg)
  "Delete package PKG.

This does not touch the package metadata at all (such as the fact
that the package is no longer installed), so that must be done
separately."
  (dired-delete-file (package-install-directory pkg)
                     ;; FIXME: query user?
                     'always))

(defun package--download-one-archive (archive)
  "Download a single archive file and cache it locally.

Downloads the archive index from ARCHIVE and store it according
to ARCHIVE's local path."
  (let* ((archive-url (concat (package-archive-url archive)
                              package-archive-contents-filename))
         (buf (url-retrieve-synchronously archive-url))
         str)
    (package-handle-response buf)
    (with-current-buffer buf
      (setq str (buffer-substring-no-properties (point-min) (point-max))))
    (make-directory (file-name-directory (package-archive-content-file archive)) t)
    (with-temp-file (package-archive-content-file archive)
      (insert str))
    (kill-buffer buf)))

(defun package-refresh-contents ()
  "Download the archive descriptions if needed.

Invoking this will ensure that Emacs knows about the latest
versions of all packages. This will let Emacs make them available
for download."
  (interactive)
  (loop for (archive ign) in package-archives
        do (package--download-one-archive archive))
  (package-read-all-archive-contents))

(defun package-activate-all-installed ()
  "Activate all installed packages."
  (loop for (name . pkgs) in package-registry
        do (loop for pkg in pkgs
                 when (eq (package-status pkg) 'installed)
                 do (package-activate pkg))))

(defun package-initialize ()
  "Load all packages and activate as many as possible."
  (package-register-all-installed)
  (package-activate-all-installed)
  (package-read-all-archive-contents))



;;;; Package menu mode.

(defvar package-menu-mode-map
  (let ((map (make-keymap))
        (menu-map (make-sparse-keymap "Package")))
    (suppress-keymap map)
    (define-key map "q" 'quit-window)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'package-menu-mark-unmark)
    (define-key map "\177" 'package-menu-backup-unmark)
    (define-key map "d" 'package-menu-mark-delete)
    (define-key map "i" 'package-menu-mark-install)
    (define-key map "g" 'package-menu-revert)
    (define-key map "r" 'package-menu-refresh)
    (define-key map "~" 'package-menu-mark-obsolete-for-deletion)
    (define-key map "x" 'package-menu-execute)
    (define-key map "h" 'package-menu-quick-help)
    (define-key map "?" 'package-menu-view-commentary)
    (define-key map [menu-bar package-menu] (cons "Package" menu-map))
    (define-key menu-map [mq]
      '(menu-item "Quit" quit-window
                  :help "Quit package selection"))
    (define-key menu-map [s1] '("--"))
    (define-key menu-map [mn]
      '(menu-item "Next" next-line
                  :help "Next Line"))
    (define-key menu-map [mp]
      '(menu-item "Previous" previous-line
                  :help "Previous Line"))
    (define-key menu-map [s2] '("--"))
    (define-key menu-map [mu]
      '(menu-item "Unmark" package-menu-mark-unmark
                  :help "Clear any marks on a package and move to the next line"))
    (define-key menu-map [munm]
      '(menu-item "Unmark backwards" package-menu-backup-unmark
                  :help "Back up one line and clear any marks on that package"))
    (define-key menu-map [md]
      '(menu-item "Mark for deletion" package-menu-mark-delete
                  :help "Mark a package for deletion and move to the next line"))
    (define-key menu-map [mi]
      '(menu-item "Mark for install" package-menu-mark-install
                  :help "Mark a package for installation and move to the next line"))
    (define-key menu-map [s3] '("--"))
    (define-key menu-map [mg]
      '(menu-item "Reload package list" package-menu-revert
                  :help "Reload the list of packages"))
    (define-key menu-map [mr]
      '(menu-item "Refresh package list" package-menu-refresh
                  :help "Download the ELPA archive"))
    (define-key menu-map [s4] '("--"))
    (define-key menu-map [mt]
      '(menu-item "Mark obsolete packages" package-menu-mark-obsolete-for-deletion
                  :help "Mark all obsolete packages for deletion"))
    (define-key menu-map [mx]
      '(menu-item "Execute actions" package-menu-execute
                  :help "Perform all the marked actions"))
    (define-key menu-map [s5] '("--"))
    (define-key menu-map [mh]
      '(menu-item "Help" package-menu-quick-help
                  :help "Show short key binding help for package-menu-mode"))
    (define-key menu-map [mc]
      '(menu-item "View Commentary" package-menu-view-commentary
                  :help "Display information about this package"))
    map)
  "Local keymap for `package-menu-mode' buffers.")

(defvar package-menu-sort-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'package-menu-sort-by-column)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for package menu sort buttons.")

(define-derived-mode package-menu-mode special-mode "Package Menu"
  "Major mode for browsing a list of packages.

Letters do not insert themselves; instead, they are commands."
  :group 'package-menu
  (setq truncate-lines t))

(defun package-menu-refresh ()
  "Download the ELPA archive.
This fetches the file describing the current contents of
the Emacs Lisp Package Archive, and then refreshes the
package menu.  This lets you see what new packages are
available for download."
  (interactive)
  (package-refresh-contents)
  (package-list-packages-internal))

(defun package-menu-revert ()
  "Update the list of packages."
  (interactive)
  (package-list-packages-internal))

(defun* package-menu-mark-internal (what &optional (pos (point)))
  "Internal function to mark a package.

WHAT is the character used to mark the line at POS."
  (save-excursion
    (goto-char pos)
    (unless (eobp)
     (let ((buffer-read-only nil))
       (beginning-of-line)
       (forward-char (package-menu-column-offset package-menu-column-command))
       (delete-char 1)
       (insert what)))))

(defun package-menu-mark-delete (&optional arg)
  "Mark a package for deletion and move to the next line.

ARG is a (currently unused) numeric argument."
  (interactive "p")
  (package-menu-mark-internal "D"))

(defun package-menu-mark-install (&optional arg)
  "Mark a package for installation and move to the next line.

ARG is a (currently unused) numeric argument."
  (interactive "p")
  (package-menu-mark-internal "I"))

(defun package-menu-mark-unmark (&optional arg)
  "Clear any marks on a package and move to the next line.

ARG is a (currently unused) numeric argument."
  (interactive "p")
  (package-menu-mark-internal " "))

(defun package-menu-backup-unmark ()
  "Back up one line and clear any marks on that package."
  (interactive)
  (forward-line -1)
  (package-menu-mark-internal " ")
  (forward-line -1))

(defun package-menu-mark-obsolete-for-deletion ()
  "Mark all obsolete packages for deletion."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (while (not (eobp))
      (if (looking-at ".*\\s obsolete\\s ")
          (package-menu-mark-internal "D")
        (forward-line 1)))))

(defun package-menu-quick-help ()
  "Show short key binding help for `package-menu-mode'."
  (interactive)
  (message "n-ext, i-nstall, d-elete, u-nmark, x-ecute, r-efresh, h-elp ?-view commentary"))

(defun package-menu-view-commentary ()
  "Display information about this package.
For single-file packages, shows the commentary section from the header.
For larger packages, shows the README file."
  (interactive)
  (let ((pkg (package-menu-make-pkg (package-menu-parse-line)))
         (buffer (get-buffer-create "*Package Info*")))
    (package-find-rest pkg)
    (with-current-buffer buffer
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (format "Package information for %s\n\n%s"
                        (package-name pkg)
                        (package-commentary pkg)))
        (goto-char (point-min)))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (view-mode))
    (display-buffer buffer t)))

(defun package-menu-execute ()
  "Perform all the marked actions.

Packages marked for installation will be downloaded and
installed. Packages marked for deletion will be removed. Note
that after installing packages you will want to restart Emacs."
  (interactive)
  (goto-char (point-min))
  (loop until (eobp)
        for info = (package-menu-parse-line)
        for pkg = (package-menu-make-pkg info)
        for cmd = (package-menu-get-command info)
        when cmd do (funcall cmd pkg))
  (package-list-packages-internal))

(defstruct (package-menu-col (:type list))
  "Specification of a single column in the package list buffer.

See `package-menu-columns' for a description of the slot types."
  name
  type
  width
  reader
  writer
  comparator
  skip-struct)

(defconst package-menu-column-command
  '(""        command   2  ignore                ignore                    ignore         t)
  "Menu definition for the \"Command\" column.")

(defconst package-menu-column-name
  '("Package"  name     20 intern                package-name              string-lessp   nil)
  "Menu definition for the \"Package\" column.")

(defconst package-menu-column-version
  '("Version"  version  12 version-to-list       package-version-canonical version-list-< nil)
  "Menu definition for the \"Version\" column.")

(defconst package-menu-column-status
  '("Status"   status   8  package-status-symbol package-status-string     string-lessp   nil)
  "Menu definition for the \"Status\" column.")

(defconst package-menu-column-summary
  '("Summary"  summary  60 identity              package-summary           string-lessp   nil)
  "Menu definition for the \"Summary\" column.")

(defcustom package-menu-columns
  (list package-menu-column-command
        package-menu-column-name
        package-menu-column-version
        package-menu-column-status
        package-menu-column-summary)
  "Specification of columns shown in the package menu.

Columns are displayed in the order in which they appear in this
variable. Note that the \"command\", \"name\" and \"version\"
columns are required to resolve a package and determine the
operation to perform on it.

Each column has the following properties:

NAME: The human-readable name of the column.

TYPE: The name of the column, as a symbol.

WIDTH: The maximum width of the column. This includes padding, so
       the number of printed characters will be strictly less
       than this.

READER: Function to use to read a Lisp object from the buffer
        line. A function which receives a single string argument
        and returns a value appropriate for assignment to a
        package structure.

WRITER: Function to write the column to a string. Takes a single
        argument, a package structure, and returns a string
        representation of the column information.

COMPARATOR: Function to compare two package structures according
            to this column. used for sorting.

SKIP-STRUCT: Skip adding this column to the package structure.
             Skips adding the column if this slot is non-nil."
  :group 'package
  :type `(repeat (choice
                  (const :tag "Command"      ,package-menu-column-command)
                  (const :tag "Package name" ,package-menu-column-name)
                  (const :tag "Version"      ,package-menu-column-version)
                  (const :tag "Status"       ,package-menu-column-status)
                  (const :tag "Summary"      ,package-menu-column-summary)
                  (list  :tag "Custom Definition"
                         (string   :tag "Name")
                         (symbol   :tag "Type")
                         (integer  :tag "Width")
                         (function :tag "Reader")
                         (function :tag "Writer")
                         (function :tag "Comparator")
                         (boolean  :tag "Skip Struct")))))

(defconst package-menu-commands '((package-install . "I")
                                  (package-delete  . "D"))
  "Commands available in the package menu.

This is an alist of the form (FUNC . STR), where FUNC is the
symbol name of the function to run and STR is the string from the
package status buffer which represents the command.")

(defun* package-print-package (pkg &optional newline (print-func (current-buffer)))
  "Insert a single PKG line for the menu buffer.

If NEWLINE is non-nil, print a newline after PKG. PRINT-FUNC is
passes as the second argument of `princ', which see."
  (let ((face
         (if (eq (package-type pkg) 'builtin)
             'font-lock-builtin-face
           (case (package-status pkg)
             (available 'default)
             ((installed activated) 'font-lock-comment-face)
             (t ; obsolete, but also the default.
                                        ; is warning ok?
              'font-lock-warning-face))))
        (line (loop for col in package-menu-columns
                    for writer-func = (package-menu-col-writer col)
                    for width = (package-menu-col-width col)
                    for col-str = (or (funcall writer-func pkg) "")
                    concat (format (format "%%-%d.%ds" width (1- width)) col-str))))

    ;; TODO: Make this `insert' instead?
    (princ (propertize line 'font-lock-face face) print-func)
    (when newline
      (princ "\n" print-func))))

(defun package-menu-parse-line (&optional buf pos)
  "Parses a package line into a plist.

BUF is the buffer and POS is the starting position at which to
begin parsing. If BUF and POS are not supplied, they default to
the current position in the current buffer.

Advances point to the end of the line."
  (setq buf (or buf (current-buffer))
        pos (or pos (point)))
  (with-current-buffer buf
    (goto-char pos)
    (beginning-of-line)
    (loop for col in package-menu-columns
          for attr = (package-menu-col-type col)
          for width = (package-menu-col-width col)
          for end-pos = (min (+ (point) width) (line-end-position))
          for raw-val = (buffer-substring-no-properties (point)
                                                        end-pos)
          for stripped = (replace-regexp-in-string
                          "\\(^[[:space:]\\n]*\\|[[:space:]\\n]*$\\)" "" raw-val)
          append (list attr stripped)
          do (goto-char end-pos)
          finally do (forward-line 1))))

(defun package-menu-make-pkg (plist)
  "Create a package structure from PLIST.

This should be a plist as returned from
`package-menu-parse-line'."
  (loop for key in plist by 'cddr
        for struct-key = (intern (concat ":" (symbol-name key)))
        for val = (plist-get plist key)
        for col = (find key package-menu-columns :key 'package-menu-col-type)
        for reader = (package-menu-col-reader col)
        unless (package-menu-col-skip-struct col)
          append (list struct-key (funcall reader val)) into result
        finally return (apply 'make-package result)))

(defun package-menu-get-command (plist)
  "Get the function specified by the command property in PLIST.

Uses the variable `package-menu-commands' to decode the command
string."
  (car (find (plist-get plist 'command)
             package-menu-commands
             :key 'cdr
             :test 'equal)))

(defun package-menu-column-offset (col)
  "Get the offset from the beginning of the line of COL.

The offset is dependent on the current value of
`package-menu-columns', so this must be recomputed each time it
is used."
  (let ((offset 0))
   (loop for current in package-menu-columns
         when (equal current col) return offset
         do (setq offset (+ offset (package-menu-col-width current))))))

(defun* package-list-packages-internal (&optional (buf (get-buffer-create "*Packages*"))
                                                  (selector 'name))
  "List the available and installed packages.

Inserts the contents into BUF, or a new buffer called
\"*Packages*\" if BUF is nil. The packages are ordered according
to SELECTOR, which is either the symbol name of a column or a
complete column specification as described by
`package-menu-columns'.

This function does not initialize or refresh the list of
packages, so that must be done separately."
  (with-current-buffer buf
    (setq buffer-read-only nil)
    (erase-buffer)
    (let* ((col (if (listp selector)
                    selector
                  (find selector package-menu-columns :key 'package-menu-col-type)))
           (comparator (package-menu-col-comparator col))
           (sort-pred '(lambda (left right)
                         (let ((vleft (package-property-get left selector))
                               (vright (package-property-get right selector)))
                           (funcall comparator vleft vright)))))
      (loop for pkg in (sort (package-registry-flat) sort-pred)
            do (package-print-package pkg t)))
    (setq buffer-read-only t)
    (goto-char (point-min)))
  buf)

(defun package-menu-sort-by-column (&optional e)
  "Sort the package menu by the last column clicked, E."
  (interactive (list last-input-event))
  (if e (mouse-select-window e))
  (let* ((pos (event-start e))
         (obj (posn-object pos))
         (col (if obj
                  (get-text-property (cdr obj) 'package-menu-col (car obj))
                (get-text-property (posn-point pos) 'package-menu-col)))))
  (package-list-packages-internal nil col))

(defun package-menu-compute-header-line ()
  "Compute a header format according to `package-menu-columns'.

This does not actually set the header line, it only creates and
returns a value suitable for `header-line-format'."
  (loop for col in package-menu-columns
        for name = (package-menu-col-name col)
        for width = (+ (or width 0) (package-menu-col-width col))
        concat (propertize name
                           'package-menu-col col
                           'help-echo "mouse-1: sort by column"
                           'mouse-face 'highlight
                           'keymap package-menu-sort-button-map)
        concat (propertize " " 'display (list 'space :align-to width)
                           'face 'fixed-pitch)))

(defun package-list-packages (&optional refresh)
  "Display a list of packages.

Does not refresh the list of packages before displaying unless a
prefix argument is supplied. The list is displayed in a buffer
named `*Packages*'."
  (interactive "P")
  (when refresh
    (package-refresh-contents))
  (with-current-buffer (package-list-packages-internal)
    (package-menu-mode)
    (setq header-line-format (package-menu-compute-header-line))

    ;; It's okay to use pop-to-buffer here.  The package menu buffer
    ;; has keybindings, and the user just typed 'M-x
    ;; package-list-packages', suggesting that they might want to use
    ;; them.
    (pop-to-buffer (current-buffer))))

;; Make it appear on the menu.
(define-key-after menu-bar-options-menu [package]
  '(menu-item "Manage Packages" package-list-packages
              :help "Install or uninstall additional Emacs packages"))

(provide 'package)

;;; package.el ends here
