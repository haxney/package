;;; package.el --- Simple package system for Emacs

;; Copyright (C) 2007, 2008, 2009 Tom Tromey <tromey@redhat.com>

;; Author: Tom Tromey <tromey@redhat.com>
;; Created: 10 Mar 2007
;; Version: 0.9.5pre
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
;;    Enters a mode similar to buffer-menu which lets you manage
;;    packages.  You can choose packages for install (mark with "i",
;;    then "x" to execute) or deletion (not implemented yet), and you
;;    can see what packages are available.  This will automatically
;;    fetch the latest list of packages from ELPA.
;;
;; M-x package-list-packages-no-fetch
;;    Like package-list-packages, but does not automatically fetch the
;;    new list of packages.
;;
;; M-x package-install-from-buffer
;;    Install a package consisting of a single .el file that appears
;;    in the current buffer.  This only works for packages which
;;    define a Version header properly; package.el also supports the
;;    extension headers Package-Version (in case Version is an RCS id
;;    or similar), and Package-Requires (if the package requires other
;;    packages).
;;
;; M-x package-install-file
;;    Install a package from the indicated file.  The package can be
;;    either a tar file or a .el file.  A tar file must contain an
;;    appropriately-named ".epkg" file; a .el file must be properly
;;    formatted as with package-install-from-buffer.

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
;; format is either  a tar file or a single .el file.

;; A tar file should be named "NAME-VERSION.tar".  The tar file must
;; unpack into a directory named after the package and version:
;; "NAME-VERSION".

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
                :value-type (group (string :tag "Archive URL")
                                   (string :tag "Local path")))
  :group 'package
  :package-version '("package.el" . "0.9.3"))

(defconst package-archive-version 2
  "Version number of the package archive understood by this file.
Lower version numbers than this will probably be understood as well.")

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
                          (builtin . special))
  "Allowed file types for package files.

A list of (TYPE-NAME . EXTENSION). The special type \"builtin\"
doesn't have an extension, since it will never be independently
downloaded or dealt with in any way aside from resolving
dependencies.")

(defstruct (package (:include elx-pkg)
                         (:constructor inherit-package
                                       (pkg
                                        &key archive type
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

The special constructor, `inherit-package' allows constructing a
`package' struct from an existing `elx-pkg' struct. Extra
arguments are supported by keys."
  archive
  type)

(defsubst package-version-canonical (pkg)
  "Return the canonical version of PKG.

Simply passes it through `elx-version-canonical'."
  (elx-version-canonical (package-version pkg)))

(defun package-suffix (pkg &optional noerror)
  "Gets the download suffix for PKG.

If PKG is a builtin package, signals an error unless NOERROR is
non-nil."
  (let ((suffix (aget package-types (package-type pkg))))
    (when (eq suffix 'special)
      (unless noerror
        (error "Package is a builtin, and therefore does not have a suffix")))
    suffix))

(defvar package-available-alist
  nil
  "Alist of all packages available for installation.

This is an alist of the form (NAME . (PACKAGE...)), where NAME is
the symbol name of a package and PACKAGE is an individual
`package' structure.

More than one package is allowed for each name, since there may
be multiple versions of a package available or two archives
may each have different versions of a package available.")

(defvar package-installed-alist nil
  "Alist of all installed packages activated.

Maps the package name to a `package' struct.")

(defvar package-activated-list nil
  "List of all activated packages.

Only one version of a package can be activated at a time.")

(defvar package-obsolete-alist nil
  "Representation of obsolete packages.
Like `package-installed-alist', but maps package name to a second alist.
The inner alist is keyed by version.")

(defun* package-find (name &key version
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
                           type)
  "Search `package-available-alist' for a package named NAME.

Returns a list of matches, since there may be more than one
package with the same name (i.e. different versions).

The optional keyword arguments allow the results to be narrowed
down to return only those packages which match all of the
supplied keywords. For example:

    (package-find 'package :version '(0 9 5))

Would return a list of packages called 'package with version
number \"0.9.5\", if any exist."
  (let ((pkgs (aget package-available-alist name)))
    (dolist (slot
             ;; This is `cddr' to skip the `name' slot, as well as the cl-tag.
             (cddr (mapcar 'car (get 'package 'cl-struct-slots)))
             pkgs)
      (when (symbol-value slot)
        (setq pkgs (remove* (symbol-value slot) pkgs
                            :test-not 'equal
                            :key (intern (concat "package-" (symbol-name slot)))))))))

;; TODO: Resolve multiple matches using archive priority?
(defun package-find-latest (name noerror &rest keys)
  "Find the newest version of package NAME.

If NOERROR is nil, signal an error when no matching package is
found, otherwise return nil.

KEYS is a set of keyword arguments to be passed to
`package-find'. If the :version keyword is present, it is
ignored.

Uses `package-find' to search for packages named NAME matching
KEYS and returns the one with the greatest version number.

If there are multiple packages with the same name and version,
only one is returned; there is no guarantee of which one that
will be."
  ;; Ignore the :version keyword; that is the entire point of this function.
  (when (plist-get keys :version)
    (setq keys (plist-put keys :version nil)))

  (let* ((pkgs (apply 'package-find name keys))
        (result (car-safe pkgs)))
    (dolist (pkg (cdr-safe pkgs))
      (when (version-list-< (package-version result) (package-version pkg))
        (setq result pkg)))
    (if (or result noerror)
        result
      (error "No package found named '%s' matching parameters '%s'" name keys))))

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
  (file-name-expand (file-name-as-directory (nth 1 (aget package-archives archive)))))

(defun package-archive-content-file (archive)
  "Returns the path of the content file of ARCHIVE.

ARCHIVE must be the symbol name of an archive."
  (concat (package-archive-localpath archive) package-archive-contents-filename))

(defun package-read-file (file)
  "Read `package' data.

FILE is the file to read. Returns a `package' structure if
successful."
  (let (str data)
    (when (and (file-readable-p file)
               (file-regular-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (setq str (buffer-string)))
      (when str
        (setq data (package-read-from-string str))))
    (apply 'make-package data)))

(defun package-register (pkg registry)
  "Register package PKG if it isn't already in REGISTRY.

Returns nil if PKG was already in the list or PKG if it was not."
  (let ((pkg-name (package-name pkg))
        (existing-pkgs (aget registry pkg-name)))
    (when existing-pkgs
      (unless (member pkg existing-pkgs)
       (nconc existing-pkgs (list pkg))
       pkg))))

(defsubst package-load-descriptor (pkg)
  "Return information the info file of PKG.

PKG can be a minimal `package' structure; only
the :name, :version, and :archive fields are needed.

Return nil if the package could not be found."
  (package-read-file (package-info-file pkg)))

(defun package-split-dirname (dir)
  "Split DIR into a name and version.

DIR must be a directory of the form \"NAME-VERSION\" which will
be split into a cons cell with the form (NAME . VERSION), where
NAME is an interned symbol and VERSION is a list as returned by
`version-to-list'. DIR can be either a relative or absolute
filename, only the last element of the filename (which should be
the directory to examine) will be considered."
  (let* ((local-dir (file-name-nondirectory (directory-file-name dir)))
         (parts (split-string local-dir "-" t)))
    (cons (intern (nth 0 parts))
          (version-to-list (nth 1 parts)))))

(defun package-from-dirname (dir)
  "Create a skeleton `package' structure from DIR.

This is mainly used to create a package with enough information
that `package-info-file' can find the info file for the package
in DIR.

Searches `package-archives' for a prefix which contains DIR and
then uses the tail directory to determine the package name and
version."
  (let ((local-dir (file-name-nondirectory (directory-file-name dir)))
        (dir-info (package-split-dirname dir))
        archive)
    (dolist (arch package-archives)
      (when (locate-dominating-file
                    (package-archive-localpath (cdr arch))
                    local-dir)
        (setq archive arch)))
    (unless archive
      (error "Could not find an archive containing dir: %s" dir))
    (make-package :name (car dir-info)
                  :version (cdr dir-info)
                  :archive archive)))

;; TODO: Add special handling of builtin packages, so that directories don't
;; need to be created for each builtin package.
(defun package-register-installed ()
  "Register metadata of all installed packages.

Uses `package-archives' to find packages."
  (mapc (lambda (archive-info)
          (let* ((archive (car archive-info))
                 (archive-dir (package-archive-localpath archive)))
            (when (and (file-readable-p archive-dir)
                     (file-directory-p archive-dir))
              (mapc (lambda (pkg-dirname)
                      (package-register (package-load-descriptor
                                         (package-from-dirname pkg-dirname))
                                        package-installed-alist))
                    (directory-files archive-dir t "^[^.]")))))
        package-archives))

(defun package-install-directory (pkg)
  "Return the install directory for PKG.

The install directory is where a particular package is (or would
be, for un-installed packages) installed. Packages are installed
within a sub-folder of their archive's local path named
\"NAME-VERSION\", where NAME is the name of the package and
VERSION is the version of the package after being processed by
`package-version-canonical'."
  (let* ((name (symbol-name (package-name pkg)))
         (version (package-version-canonical pkg))
         (archive-dir (if (eq name 'package)
                          ;; The package for package.el is handled specially.
                          package-user-dir
                          (package-archive-localpath (package-archive pkg))))
         (raw-name (format "%s/%s-%s" archive-dir name version)))
    (convert-standard-filename (file-name-as-directory (expand-file-name
                                                        raw-name)))))

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

(defsubst package-info-file (pkg)
  "Returns the info (.epkg) file for PKG."
  (concat (package-install-directory pkg) package-info-filename))

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
(defun package-do-activate (package)
  "Set up a single PACKAGE after it has been installed.

Modifies `load-path' to include the package directory and loads
the `autoload' file for the package."
  (let* ((pkg-name (package-name package))
         (pkg-dir (package-install-directory package)))

    (add-to-list 'load-path pkg-dir)
    ;; Load the autoloads and activate the package.
    (load (package-autoload-file package) nil t)
    (add-to-list 'package-activated-list package)
    ;; Don't return nil.
    t))

;; FIXME: return a reason instead?
(defun package-activate (package)
  "Try to activate PACKAGE.

Signal an error if the package could not be activated.

Recursively activates all dependencies of PACKAGE."
  ;; Assume the user knows what he is doing -- go ahead and activate a
  ;; newer version of a package if an older one has already been
  ;; activated.  This is not ideal; we'd at least need to check to see
  ;; if the package has actually been loaded, and not merely
  ;; activated.
  (let ((name (package-name package))
        (hard-reqs (package-required-packages package 'hard))
        (soft-reqs (package-required-packages package 'soft)))
    (cond
     ;; Don't try to activate 'emacs', that's just silly.
     ((eq name 'emacs))
     ;; If this package is already the most recently installed version, no
     ;; further action is needed.
     ((equal package (package-find-latest name t)))
     ((member package package-activated-list))
     (t
      ;; Signal an error if a hard requirement cannot be found, but not for a
      ;; soft requirement.
      (dolist (req hard-reqs)
        (package-activate (package-find-latest req nil)))
      (dolist (req soft-reqs)
        (package-activate (package-find-latest req t)))))))

;; TODO: CL-CHECK
(defun package-mark-obsolete (package pkg-vec)
  "Put PACKAGE on the obsolete list, if not already there.

PKG-VEC describes the version of PACKAGE to mark obsolete."
  (let ((elt (assq package package-obsolete-alist)))
    (if elt
        ;; If this obsolete version does not exist in the list, update
        ;; it the list.
        (unless (assoc (package-version pkg-vec) (cdr elt))
          (setcdr elt (cons (cons (package-version pkg-vec) pkg-vec)
                            (cdr elt))))
      ;; Make a new association.
      (setq package-obsolete-alist
            (cons (cons package (list (cons (package-version pkg-vec)
                                            pkg-vec)))
                  package-obsolete-alist)))))

(defun package-generate-autoloads (pkg)
  "Generate autoload definitions for PKG."
  (let ((generated-autoload-file (package-autoload-file pkg))
         (version-control 'never))
    ;; In Emacs 22 `update-directory-autoloads' does not seem
    ;; to be autoloaded...
    (require 'autoload)
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

  (require 'tar-mode)
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

(defun package-unpack-tar (pkg &optional buf)
  "Unpack a tar PKG from BUF or the current buffer.

BUF is expected to contain a tarred package archive. If BUF is
nil, the current buffer is used."
  (let ((buf (or buf (current-buffer)))
        (pkg-dir (package-install-directory pkg))
        (pkg-info-file (package-info-file pkg)))

    (make-directory pkg-dir t)
    (if (file-directory-p pkg-dir)
        (mapc (lambda (file) nil) ;; TODO: 'delete-file when we're more confident
              (directory-files pkg-dir t "^[^.]")))
    (package-untar-buffer buf (package-parent-directory pkg-dir))
    ;; Write the "info.epkg" file.
    (unless (file-exists-p pkg-info-file)
     (with-temp-file pkg-info-file
       (insert (cl-merge-pp pkg))))
    (package-generate-autoloads pkg)
    (let ((load-path (cons pkg-dir load-path)))
      (byte-recompile-directory pkg-dir 0 t))))

(defun package-unpack-single (pkg &optional buf)
  "Install PKG from contents of buf or the current buffer.

PKG is the package metadata and BUF is the buffer from which to
install the package. If BUF is nil, then use the current buffer."
  (let ((buf (or buf (current-buffer)))
        (pkg-dir (package-install-directory pkg))
        (pkg-file (package-install-file-path pkg))
        (pkg-info-file (package-info-file pkg)))
    (when (and (not (eq (package-type pkg) 'package)) (file-exists-p pkg-file))
      (error "Destination file %s exists, refusing to overwrite" pkg-file))

    (make-directory pkg-dir t)
    (with-temp-file pkg-file
      (with-current-buffer buf
        (buffer-substring)))
    ;; Write the "info.epkg" file.
    (with-temp-file pkg-info-file
      (insert (cl-merge-pp pkg)))
    (package-generate-autoloads file-name pkg-dir)
    (let ((load-path (cons pkg-dir load-path)))
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
              (package-download-url pkg)))
        (unpacker (intern (format "packge-unpack-%s" (package-type pkg)))))
    (package-handle-response buf)
    (apply unpacker (list pkg buf))
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
  (let ((hard (loop for (name provides)
                    in (package-requires-hard pkg)
                    collect (package-find-latest
                             name nil :provides provides)))
        (soft (loop for (name provides)
                    in (package-requires-soft pkg)
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
        append (package-compute-transaction result
                                            (package-required-packages req))
        into temp
        finally return (append temp result)))

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

(defun package--read-archive-file (archive)
  "Re-read archive file FILE, if it exists.

Will return the data from the file, or nil if the file does not
exist. Will signal an error if the archive version is not
supported."
  (let ((file (package-archive-content-file archive)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents-literally file)
          (let ((contents (package-read-from-string
                           (buffer-substring-no-properties (point-min)
                                                           (point-max)))))
            (if (/= (car contents) package-archive-version)
                (error "Package archive version %d is not equal to %d - upgrade package.el"
                       (car contents) package-archive-version))
            (cdr contents))))))

(defun package-read-all-archive-contents ()
  "Read the archive description of each of the archives in `package-archives'."
  (loop for (archive info) in package-archives
        do (package-read-archive-contents archive)))

(defun package-read-archive-contents (archive)
  "Re-read `archive-contents' for ARCHIVE.

Will add any new packages found `package-available-alist'. Will
signal an error if the archive version is too new or if a
package's :archive field does not match ARCHIVE."
  (let ((archive-contents (package--read-archive-file archive)))
    (if archive-contents
        (dolist (pkg archive-contents)
          (unless (eq (package-archive pkg) archive)
            (error "Package %s lists %s as its archive, but was read from archive %s"
                   (package-name pkg)
                   (package-archive pkg)
                   archive))
          (package-register pkg package-available-alist)))))

(defun package-download-transaction (transaction)
  "Download and install all the packages in the given TRANSACTION."
  (loop for pkg in transaction
        do (package-download pkg)))

(defun package-install (name &optional version)
  "Install the package named NAME at VERSION.

NAME must be a symbol.

If VERSION is not given, it defaults to the most recent version
according to `package-find-latest'.

Interactively, prompts for the package name."
  (interactive
   (list (intern (completing-read "Install package: "
                                 (mapcar (lambda (elt)
                                           (symbol-name (car elt)))
                                         package-available-alist)
                                 nil t))))
  (unless version
    (setq version (package-version (package-find-latest name))))

  (let ((pkg (package-find name :version version)))
    (unless pkg
      (error "Package '%s', version '%s' not available for installation"
             name version))
    (let ((transaction
           (package-compute-transaction (list name)
                                        (package-requires-hard pkg))))
      (package-download-transaction transaction)))
  ;; Try to activate it.
  (package-initialize))

;; TODO: CL-CHECK
(defun package-strip-rcs-id (v-str)
  "Strip RCS version ID from the version string V-STR.

If the result looks like a dotted numeric version, return it.
Otherwise return nil."
  (if v-str
      (if (string-match "[ \t]*\\$\\(?:Revision\\|Id\\):[ \t]\\(?:[^ \t]+,v[ \t]+\\)?\\([0-9.]+\\).*\\$$" v-str)
          (match-string 1 v-str)
        (if (string-match "^[0-9.]*$" v-str)
            v-str))))

;; TODO: CL-CHECK
(defun package-buffer-info ()
  "Return a vector of information about the package in the current buffer.
The vector looks like [FILENAME REQUIRES DESCRIPTION VERSION COMMENTARY]
FILENAME is the file name, a string.  It does not have the \".el\" extension.
REQUIRES is a requires list, or nil.
DESCRIPTION is the package description (a string).
VERSION is the version, a string.
COMMENTARY is the commentary section, a string, or nil if none.
Throws an exception if the buffer does not contain a conforming package.
If there is a package, narrows the buffer to the file's boundaries.
May narrow buffer or move point even on failure."
  (goto-char (point-min))
  (if (re-search-forward "^;;; \\([^ ]*\\)\\.el --- \\(.*\\)$" nil t)
      (let ((file-name (match-string 1))
            (desc (match-string 2))
            (start (progn (beginning-of-line) (point))))
        (if (search-forward (concat ";;; " file-name ".el ends here"))
            (progn
              ;; Try to include a trailing newline.
              (forward-line)
              (narrow-to-region start (point))
              (require 'lisp-mnt)
              ;; Use some headers we've invented to drive the process.
              (let* ((requires-str (lm-header "package-requires"))
                     (requires (if requires-str
                                   (package-read-from-string requires-str)))
                     ;; Prefer Package-Version, because if it is
                     ;; defined the package author probably wants us
                     ;; to use it.  Otherwise try Version.
                     (pkg-version
                      (or (package-strip-rcs-id (lm-header "package-version"))
                          (package-strip-rcs-id (lm-header "version"))))
                     (commentary (lm-commentary)))
                (unless pkg-version
                  (error
                   "Package does not define a usable \"Version\" or \"Package-Version\" header"))
                ;; Turn string version numbers into list form.
                (setq requires
                      (mapcar
                       (lambda (elt)
                         (list (car elt)
                               (package-version-split (car (cdr elt)))))
                       requires))
                (set-text-properties 0 (length file-name) nil file-name)
                (set-text-properties 0 (length pkg-version) nil pkg-version)
                (set-text-properties 0 (length desc) nil desc)
                (vector file-name requires desc pkg-version commentary)))
          (error "Package missing a terminating comment")))
    (error "No starting comment for package")))

;; TODO: CL-CHECK
(defun package-tar-file-info (file)
  "Find package information for a tar file.
FILE is the name of the tar file to examine.
The return result is a vector like `package-buffer-info'."
  (setq file (expand-file-name file))
  (unless (string-match "^\\(.+\\)-\\([0-9.]+\\)\\.tar$" file)
    (error "`%s' doesn't have a package-ish name" file))
  (let* ((pkg-name (file-name-nondirectory (match-string-no-properties 1 file)))
         (pkg-version (match-string-no-properties 2 file))
         ;; Extract the package descriptor.
         (pkg-def-contents (shell-command-to-string
                            ;; Requires GNU tar.
                            (concat "tar -xOf " file " "
                                    pkg-name "-" pkg-version "/"
                                    pkg-name ".epkg")))
         (pkg-def-parsed (package-read-from-string pkg-def-contents)))
    ;; TODO: Handle `pkg-def-parsed' better.
    (let ((name-str (nth 1 pkg-def-parsed))
          (version-string (nth 2 pkg-def-parsed))
          (docstring (nth 3 pkg-def-parsed))
          (requires (nth 4 pkg-def-parsed))

          (readme (shell-command-to-string
                   ;; Requires GNU tar.
                   (concat "tar -xOf " file " "
                           pkg-name "-" pkg-version "/README"))))
      (unless (equal pkg-version version-string)
        (error "Inconsistent versions!"))
      (unless (equal pkg-name name-str)
        (error "Inconsistent names!"))
      ;; Kind of a hack.
      (if (string-match ": Not found in archive" readme)
          (setq readme nil))
      ;; Turn string version numbers into list form.
      (if (eq (car requires) 'quote)
          (setq requires (car (cdr requires))))
      (setq requires
            (mapcar
             (lambda (elt)
               (list (car elt)
                     (package-version-split (car (cdr elt)))))
             requires))
      (vector pkg-name requires docstring version-string readme))))

;; TODO: CL-CHECK
(defun package-install-buffer-internal (pkg-info type)
  "Download and install a single package.

PKG-INFO describes the package to be installed.

TYPE is either `single' or `tar'."
  (save-excursion
    (save-restriction
      (let* ((file-name (aref pkg-info 0))
             (requires (aref pkg-info 1))
             (desc (if (string= (aref pkg-info 2) "")
                       "No description available."
                     (aref pkg-info 2)))
             (pkg-version (aref pkg-info 3)))
        ;; Download and install the dependencies.
        (let ((transaction (package-compute-transaction nil requires)))
          (package-download-transaction transaction))
        ;; Install the package itself.
        (cond
         ((eq type 'single)
          (package-unpack-single file-name pkg-version desc requires))
         ((eq type 'tar)
          (package-unpack-tar (intern file-name) pkg-version))
         (t
          (error "Unknown type: %s" (symbol-name type))))
        ;; Try to activate it.
        (package-initialize)))))

(defun package-install-from-buffer (buf)
  "Install a package from the current buffer.
The package is assumed to be a single .el file which
follows the elisp comment guidelines; see
info node `(elisp)Library Headers'."
  (interactive "bInstall package from buffer:")
  (package-install-buffer-internal (inherit-package (elx-package-metadata buf)
                                                    :archive 'manual
                                                    :type 'single)))

;; TODO: CL-CHECK
(defun package-install-file (file)
  "Install a package from a FILE.
The file can either be a tar file or an Emacs Lisp file."
  (interactive "fPackage file name: ")
  (with-temp-buffer
    (insert-file-contents-literally file)
    (cond
     ((string-match "\\.el$" file) (package-install-from-buffer))
     ((string-match "\\.tar$" file)
      (package-install-buffer-internal (package-tar-file-info file) 'tar))
     (t (error "Unrecognized extension `%s'" (file-name-extension file))))))

;; TODO: CL-CHECK
(defun package-delete (name version)
  "Delete package NAME at VERSION."
  (require 'dired)          ; for dired-delete-file
  (dired-delete-file (concat (file-name-as-directory package-user-dir)
                             name "-" version)
                     ;; FIXME: query user?
                     'always))

;; TODO: CL-CHECK
(defun package--encode (string)
  "Encode a STRING by replacing some characters with XML entities."
  ;; We need a special case for translating "&" to "&amp;".
  (let ((index))
    (while (setq index (string-match "[&]" string index))
      (setq string (replace-match "&amp;" t nil string))
      (setq index (1+ index))))
  (while (string-match "[<]" string)
    (setq string (replace-match "&lt;" t nil string)))
  (while (string-match "[>]" string)
    (setq string (replace-match "&gt;" t nil string)))
  (while (string-match "[']" string)
    (setq string (replace-match "&apos;" t nil string)))
  (while (string-match "[\"]" string)
    (setq string (replace-match "&quot;" t nil string)))
  string)

;; TODO: CL-CHECK
(defun package--update-file (file location text)
  "Update FILE by finding LOCATION and inserting TEXT."
  (save-excursion
    (let ((old-buffer (find-buffer-visiting file)))
      (with-current-buffer (let ((find-file-visit-truename t))
                             (or old-buffer (find-file-noselect file)))
        (goto-char (point-min))
        (search-forward location)
        (forward-line)
        (insert text)
        (let ((file-precious-flag t))
          (save-buffer))
        (unless old-buffer
          (kill-buffer (current-buffer)))))))

;; TODO: CL-CHECK
(defun package-archive-for (name)
  "Return the archive containing the package NAME."
  (let ((desc (aget package-available-alist (intern-soft name))))
    (cdr (assoc (aref desc (- (length desc) 1)) package-archives))))

;; TODO: CL-CHECK
(defun package--download-one-archive (archive file)
  "Download a single archive file and cache it locally.

Downloads the archive index from ARCHIVE and stores it in FILE."
  (let* ((archive-name (symbol-name (car archive)))
         (archive-url (cdr archive))
         (buf (url-retrieve-synchronously (concat archive-url file))))
    (package-handle-response buf)
    (make-directory (concat (file-name-as-directory package-user-dir)
                            "archives/" archive-name) t)
    (setq buffer-file-name (concat (file-name-as-directory package-user-dir)
                                   "archives/" archive-name "/" file))
    (let ((version-control 'never))
      (save-buffer))
    (kill-buffer buf)))

;; TODO: CL-CHECK
(defun package-refresh-contents ()
  "Download the ELPA archive description if needed.
Invoking this will ensure that Emacs knows about the latest versions
of all packages.  This will let Emacs make them available for
download."
  (interactive)
  (dolist (archive package-archives)
    (package--download-one-archive archive "archive-contents"))
  (package-read-all-archive-contents))

;; TODO: CL-CHECK
(defun package-initialize ()
  "Load all packages and activate as many as possible."
  (setq package-obsolete-alist nil)
  (package-register-installed)
  (package-read-all-archive-contents)
  ;; Try to activate all our packages.
  (mapc (lambda (elt)
          (package-activate (car elt) (package-version (cdr elt))))
        package-installed-alist))



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
      '(menu-item "Update package list" package-menu-revert
		  :help "Update the list of packages"))
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

(put 'package-menu-mode 'mode-class 'special)

;; TODO: CL-CHECK
(defun package-menu-mode ()
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map package-menu-mode-map)
  (setq major-mode 'package-menu-mode)
  (setq mode-name "Package Menu")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  ;; Support Emacs 21.
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'package-menu-mode-hook)
    (run-hooks 'package-menu-mode-hook)))

;; TODO: CL-CHECK
(defun package-menu-refresh ()
  "Download the ELPA archive.
This fetches the file describing the current contents of
the Emacs Lisp Package Archive, and then refreshes the
package menu.  This lets you see what new packages are
available for download."
  (interactive)
  (package-refresh-contents)
  (package-list-packages-internal))

;; TODO: CL-CHECK
(defun package-menu-revert ()
  "Update the list of packages."
  (interactive)
  (package-list-packages-internal))

;; TODO: CL-CHECK
(defun package-menu-mark-internal (what)
  "Internal function to mark a package.

WHAT is the character used to mark the line."
  (unless (eobp)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert what)
      (forward-line))))

;; TODO: CL-CHECK
(defun package-menu-mark-delete (&optional arg)
  "Mark a package for deletion and move to the next line.

ARG is a (currently unused) numeric argument."
  (interactive "p")
  (package-menu-mark-internal "D"))

;; TODO: CL-CHECK
(defun package-menu-mark-install (&optional arg)
  "Mark a package for installation and move to the next line.

ARG is a (currently unused) numeric argument."
  (interactive "p")
  (package-menu-mark-internal "I"))

;; TODO: CL-CHECK
(defun package-menu-mark-unmark (&optional arg)
  "Clear any marks on a package and move to the next line.

ARG is a (currently unused) numeric argument."
  (interactive "p")
  (package-menu-mark-internal " "))

;; TODO: CL-CHECK
(defun package-menu-backup-unmark ()
  "Back up one line and clear any marks on that package."
  (interactive)
  (forward-line -1)
  (package-menu-mark-internal " ")
  (forward-line -1))

;; TODO: CL-CHECK
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

;; TODO: CL-CHECK
(defun package-menu-quick-help ()
  "Show short key binding help for `package-menu-mode'."
  (interactive)
  (message "n-ext, i-nstall, d-elete, u-nmark, x-ecute, r-efresh, h-elp ?-view commentary"))

;; TODO: CL-CHECK
(defun package-menu-view-commentary ()
  "Display information about this package.
For single-file packages, shows the commentary section from the header.
For larger packages, shows the README file."
  (interactive)
  (let* (start-point ok
                     (pkg-name (package-menu-get-package))
                     (buffer (url-retrieve-synchronously
                              (concat (package-archive-for pkg-name)
                                      pkg-name "-readme.txt"))))
    (with-current-buffer buffer
      ;; FIXME: it would be nice to work with any URL type.
      (setq start-point url-http-end-of-headers)
      (setq ok (eq (url-http-parse-response) 200)))
    (let ((new-buffer (get-buffer-create "*Package Info*")))
      (with-current-buffer new-buffer
        (let ((buffer-read-only nil))
          (erase-buffer)
          (insert "Package information for " pkg-name "\n\n")
          (if ok
              (insert-buffer-substring buffer start-point)
            (insert "This package does not have a README file or commentary comment.\n"))
          (goto-char (point-min))
          (view-mode)))
      (display-buffer new-buffer t))))

(defun package-menu-get-package ()
  "Return the name of the package on the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ". \\([^ \t]*\\)")
        (match-string-no-properties 1))))

;; TODO: CL-CHECK
(defun package-menu-get-version ()
  "Return the version of the package on the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ". [^ \t]*[ \t]*\\([0-9.]*\\)")
        (match-string 1))))

;; TODO: CL-CHECK
(defun package-menu-get-status ()
  "Get the status of the current line."
  (save-excursion
    (if (looking-at ". [^ \t]*[ \t]*[^ \t]*[ \t]*\\([^ \t]*\\)")
        (match-string 1)
      "")))

;; TODO: CL-CHECK
(defun package-menu-execute ()
  "Perform all the marked actions.
Packages marked for installation will be downloaded and
installed.  Packages marked for deletion will be removed.
Note that after installing packages you will want to restart
Emacs."
  (interactive)
  (goto-char (point-min))
  (forward-line 2)
  (while (not (eobp))
    (let ((cmd (char-after))
          (pkg-name (package-menu-get-package))
          (pkg-vers (package-menu-get-version))
          (pkg-status (package-menu-get-status)))
      (cond
       ((eq cmd ?D)
        (when (and (string= pkg-status "installed")
                   (string= pkg-name "package"))
          ;; FIXME: actually, we could be tricky and remove all info.
          ;; But that is drastic and the user can do that instead.
          (error "Can't delete most recent version of `package'"))
        ;; Ask for confirmation here?  Maybe if package status is ""?
        ;; Or if any lisp from package is actually loaded?
        (message "Deleting %s-%s..." pkg-name pkg-vers)
        (package-delete pkg-name pkg-vers)
        (message "Deleting %s-%s... done" pkg-name pkg-vers))
       ((eq cmd ?I)
        (package-install (intern pkg-name) (version-to-list pkg-vers)))))
    (forward-line))
  (package-menu-revert))

;; TODO: CL-CHECK
(defun package-print-package (package version key desc)
  "Print out a single PACKAGE line for the menu buffer.

PACKAGE is the package name as a symbol.

VERSION is the version as an integer vector.

KEY is the installation status of the package; either
\"available\" or \"installed\".

DESC is the short description of the package."
  (let ((face
         (cond ((eq package 'emacs) 'font-lock-builtin-face)
               ((string= key "available") 'default)
               ((string= key "installed") 'font-lock-comment-face)
               (t ; obsolete, but also the default.
                                        ; is warning ok?
                'font-lock-warning-face))))
    (insert (propertize "  " 'font-lock-face face))
    (insert (propertize (symbol-name package) 'font-lock-face face))
    (indent-to 20 1)
    (insert (propertize (package-version-join version) 'font-lock-face face))
    (indent-to 30 1)
    (insert (propertize key 'font-lock-face face))
    ;; FIXME: this 'when' is bogus...
    (when desc
      (indent-to 41 1)
      (insert (propertize desc 'font-lock-face face)))
    (insert "\n")))

;; TODO: CL-CHECK
(defun package-list-maybe-add (package status result)
  "Add PACKAGE to the list if it is not already there.

PACKAGE is the package structure.

STATUS is the installation status of the package, either
\"available\" or \"installed\".

RESULT is the list to which to add the package."
  (let* ((pkg-name (package-name package))
         (pkg-version (package-version package))
         (pkg-summary (package-summary package))
         (elt (assoc (cons pkg-name pkg-version) result)))
    (unless elt
      (setq result (cons (list (cons pkg-name pkg-version) status pkg-summary)
                         result))))
  result)

;; This decides how we should sort; nil means by package name.
(defvar package-menu-sort-key nil)

;; TODO: CL-CHECK
(defun package-list-packages-internal ()
  "List the available and installed packages."
  (package-initialize)          ; FIXME: do this here?
  (with-current-buffer (get-buffer-create "*Packages*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((info-list))
      (mapc (lambda (elt)
              (setq info-list
                    (package-list-maybe-add (cdr elt)
                                            ;; FIXME: it turns out to
                                            ;; be tricky to see if
                                            ;; this package is
                                            ;; presently activated.
                                            ;; That is lame!
                                            "installed"
                                            info-list)))
            package-installed-alist)
      (mapc (lambda (elt)
              (setq info-list
                    (package-list-maybe-add (cdr elt)
                                            "available"
                                            info-list)))
            package-available-alist)
      (mapc (lambda (elt)
              (mapc (lambda (inner-elt)
                      (setq info-list
                            (package-list-maybe-add (cdr inner-elt)
                                                    "obsolete"
                                                    info-list)))
                    (cdr elt)))
            package-obsolete-alist)
      (let ((selector (cond
                       ((string= package-menu-sort-key "Version")
                        ;; FIXME this doesn't work.
                        #'(lambda (e) (cdr (car e))))
                       ((string= package-menu-sort-key "Status")
                        #'(lambda (e) (car (cdr e))))
                       ((string= package-menu-sort-key "Description")
                        #'(lambda (e) (car (cdr (cdr e)))))
                       (t ; "Package" is default.
                        #'(lambda (e) (symbol-name (car (car e))))))))
        (setq info-list
              (sort info-list
                    (lambda (left right)
                      (let ((vleft (funcall selector left))
                            (vright (funcall selector right)))
                        (string< vleft vright))))))
      (mapc (lambda (elt)
              (package-print-package (car (car elt))
                                     (cdr (car elt))
                                     (car (cdr elt))
                                     (car (cdr (cdr elt)))))
            info-list))
    (goto-char (point-min))
    (current-buffer)))

;; TODO: CL-CHECK
(defun package-menu-sort-by-column (&optional e)
  "Sort the package menu by the last column clicked, E."
  (interactive (list last-input-event))
  (if e (mouse-select-window e))
  (let* ((pos (event-start e))
         (obj (posn-object pos))
         (col (if obj
                  (get-text-property (cdr obj) 'column-name (car obj))
                (get-text-property (posn-point pos) 'column-name))))
    (setq package-menu-sort-key col))
  (package-list-packages-internal))

;; TODO: CL-CHECK
(defun package--list-packages ()
  "Display a list of packages.
Helper function that does all the work for the user-facing functions."
  (with-current-buffer (package-list-packages-internal)
    (package-menu-mode)
    ;; Set up the header line.
    (setq header-line-format
          (mapconcat
           (lambda (pair)
             (let ((column (car pair))
                   (name (cdr pair)))
               (concat
                ;; Insert a space that aligns the button properly.
                (propertize " " 'display (list 'space :align-to column)
                            'face 'fixed-pitch)
                ;; Set up the column button.
                (if (string= name "Version")
                    name
                  (propertize name
                              'column-name name
                              'help-echo "mouse-1: sort by column"
                              'mouse-face 'highlight
                              'keymap package-menu-sort-button-map)))))
           ;; We take a trick from buff-menu and have a dummy leading
           ;; space to align the header line with the beginning of the
           ;; text.  This doesn't really work properly on Emacs 21,
           ;; but it is close enough.
           '((0 . "")
             (2 . "Package")
             (20 . "Version")
             (30 . "Status")
             (41 . "Description"))
           ""))

    ;; It's okay to use pop-to-buffer here.  The package menu buffer
    ;; has keybindings, and the user just typed 'M-x
    ;; package-list-packages', suggesting that they might want to use
    ;; them.
    (pop-to-buffer (current-buffer))))

;; TODO: CL-CHECK
(defun package-list-packages ()
  "Display a list of packages.
Fetches the updated list of packages before displaying.
The list is displayed in a buffer named `*Packages*'."
  (interactive)
  (package-refresh-contents)
  (package--list-packages))

;; TODO: CL-CHECK
(defun package-list-packages-no-fetch ()
  "Display a list of packages.
Does not fetch the updated list of packages before displaying.
The list is displayed in a buffer named `*Packages*'."
  (interactive)
  (package--list-packages))

;; Make it appear on the menu.
(define-key-after menu-bar-options-menu [package]
  '(menu-item "Manage Packages" package-list-packages
              :help "Install or uninstall additional Emacs packages"))

(provide 'package)

;;; package.el ends here
