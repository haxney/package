;;; package-maint.el --- Tools for curating the package archive

;; Copyright (C) 2010 Phil Hagelberg <technomancy@gmail.com>

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Created: 2 Jan 2009
;; Version: 0.9.3
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

;; This file allows a curator to publish an archive of Emacs packages.

;; The archive is generated from an index, which contains a list of
;; projects and repositories from which to get them. The term
;; "package" here is used to mean a specific version of a project that
;; is prepared for download and installation.

;;; History:
;;
;; Initial code written by Phil Hagelberg, multi-file package support added by
;; Dan Hackney.

;;; Code:

;; Since this library is not meant to be loaded by users
;; at runtime, use of cl functions should not be a problem.
(require 'cl)

(require 'package)

;;;###autoload
(defcustom package-base "~/src/package.el/"
  "The directory in which to store package-maint files."
  :type 'directory
  :group 'package-maint)

(defvar package-index (concat package-base "index.el")
  "The listing of all projects and repositories to get them from.
Should contain an list of projects, each formatted as a list with
the project name followed by the DVCS repository URL.")

(defvar package-working-dir (concat package-base "working/%s")
  "Directory in which to keep project checkouts.")

(defvar package-public-dir (concat package-base "public")
  "Directory in which to place packages created.")

(defvar package-version-format "^\\([0-9\\.]+[0-9]\\)*$"
  "A regex that will only match tags which indicate versions.")

(defvar package-file-types '((single . "el")
                             (tar . "tar"))
  "Allowed file types for package files.

A list of (TYPE-NAME . EXTENSION).")

(defstruct (pkg-info (:type vector))
  "Info structure which will be dumped to an 'archive-contents' file."
  version
  requires
  desc
  type)

(defstruct (pkg-buf-info (:type vector))
  "The package info returned by `package-buffer-info'.

\"package.el\" does not use the CL library, so this type is defined
here."
  filename
  requires
  desc
  version
  commentary)

(defstruct (project (:type list))
  "A project structure, read from the package index file."
  name
  url)

;;;###autoload
(defun package-build-archive ()
  "Build packages for every version of every project in the index."
  (interactive)
  (save-window-excursion
    (with-temp-buffer
      (insert-file-contents-literally package-index)
     (let ((original-dir default-directory)
           (projects (package-read-from-string
                      (buffer-substring-no-properties (point-min)
                                                      (point-max)))))
       (dolist (project projects)
         (package-build-packages project))
       (package-build-archive-contents projects)
       (cd original-dir)))))

(defun package-build-packages (project)
  "Given a PROJECT, create packages for each version needs building."
  (let ((name (project-name project)))
    (package-init project)
    (cd (package-local-checkout-dir name))
    (shell-command "git fetch --tags")
    (dolist (version (package-list-versions))
      (when (not (package-built? name version))
        (package-build-package name version (package-build-type version))))))

(defun package-build-type (version)
  "Determines whether to build a single- or multi-file package.

A VERSION is deemed worthy of a multi-file package if it contains
more than one elisp file.

Returns a type from `package-file-types'."
  (let ((command (concat "git ls-tree --name-only -r " version)))
    (with-temp-buffer
      (shell-command command (current-buffer))
      (goto-char (point-min))
      (re-search-forward "^.*\\.el$" nil t)
      ;; If a second elisp file is found, use a multi-file package.
      (if (re-search-forward "^.*\\.el$" nil t)
          'tar
        'single))))

(defun* package-build-package (name version &optional (type 'single))
  "Given a project version, create a package for it.

TYPE may be one of the keys of `package-file-types', `single' by
default."
  (message "Building %s v%s" name version)
  (cond
   ((eq type 'single)
    (package-build-single name version))
   ((eq type 'tar)
    (package-build-tar name version))
   (t
    (error "Unknown package type `%s'" type))))

(defun package-build-single (name version)
  "Create a package from a single file.

Create a package for project NAME and VERSION."
  (let* ((extension (cdr (assq 'single package-file-types)))
         (package-source (format "%s/%s.%s"
                                 (package-local-checkout-dir name)
                                 name
                                 extension)))
    (cd (package-local-checkout-dir name))
    (shell-command (format "git checkout %s" version))
    (if (not (file-exists-p package-source))
        (message "Skipping %s since %s was not found." name package-source)
      (find-file package-source)
      (package-write-buffer extension)
      (message "Built %s version %s." name version)
      (kill-buffer))))

(defun package-write-buffer (extension)
  "Write a package whose contents are in the current buffer.

EXTENSION is the file extension to append to the end of the file
name. Currently, there is only \"el\" for single files and
\"tar\" for multi-file packages."
  (save-excursion
    (save-restriction
      (let* ((info (package-buffer-info))
             (pkg-version (pkg-buf-info-version info))
             (file-name (pkg-buf-info-filename info)))
        (make-directory package-public-dir t)
        (write-region (point-min) (point-max)
                      (concat package-public-dir "/"
                              file-name "-" pkg-version "." extension)
                      nil nil nil)
        ;; special-case "package": write a second copy so that the
        ;; installer can easily find the latest version.
        (if (string= file-name "package")
            (write-region (point-min) (point-max)
                          (concat package-public-dir "/"
                                  file-name ".el")
                          nil nil nil 'ask))))))

(defun package-build-tar (name version)
  "Build package NAME for VERSION from a multi-file repository.

Returns a cons-cell containing the exit code and the process's
stderr. When successful, the stderr will contain a
newline-delimited list of files and directories added to the
archive."
  (make-directory package-public-dir t)
  (let* ((extension (cdr (assq 'tar package-file-types)))
         (prefix (concat name "-" version "/"))
         (manifest (concat prefix name "-pkg.el"))
         (dir (concat "--git-dir="
                      (expand-file-name (package-local-repo-dir name))))
         (format (concat "--format=" extension))
         (prefix-arg (concat "--prefix=" prefix))
         (output-file (concat package-public-dir "/"
                              name "-" version "." extension))
         (output (concat "--output=" (expand-file-name output-file)))
         (verbose "--verbose")
         (command (mapconcat 'identity (list "git"
                                             dir
                                             "archive"
                                             format
                                             prefix-arg
                                             output
                                             verbose
                                             version)
                             " "))
         retval
         result
         files)

    (with-temp-buffer
      (setq retval (shell-command command nil (current-buffer))
            result (buffer-substring-no-properties (point-min) (point-max))))
    ;; Generate a simple manifest if the package did not already include one.
    (when (eq retval 0)
      (setq files (split-string result))
      (unless (member manifest files)
        (package-append-manifest output-file name version)))

    (cons retval result)))

(defun package-append-manifest (archive name version &optional desc requirements)
  "Generate and append a simple manifest file.

Multi-file packages must have have a simple file which contains a
call to `define-package'. ARCHIVE is the path to the package
archive to modify. The required arguments NAME and VERSION are
included in the manifest file (and are used to determine the file
path within the archive), while DESC and REQUIREMENTS are added
if available."
  (let* ((manifest-dir (concat name "-" version "/"))
         (manifest (concat manifest-dir name "-pkg.el"))
         (append "--append")
         (file (concat "--file=" (expand-file-name archive)))
         (command (mapconcat 'identity
                             (list "tar"
                                   append
                                   file
                                   manifest)
                             " ")))
    (make-directory manifest-dir)
    (with-temp-file manifest
      (insert (pp-to-string (list 'define-package
                                  name
                                  version
                                  desc
                                  requirements))))
    (shell-command command)
    (delete-file manifest)
    (delete-directory manifest-dir)))

(defun package-init (project)
  "Create a new checkout of a PROJECT if necessary."
  (when (not (file-exists-p (package-local-checkout-dir (project-name project))))
    (make-directory (format package-working-dir (project-name project)) t)
    (cd (format package-working-dir ""))
    (shell-command (format "git clone %s %s" (project-url project) (project-name project)))))

(defun package-local-checkout-dir (name)
  "Return the working directory for project NAME."
  (format package-working-dir name))

(defun package-local-repo-dir (name)
  "Return the repository directory for package NAME."
  (concat (package-local-checkout-dir name) "/.git"))

(defun package-list-versions ()
  "List all versions of a project. Must run in project checkout."
  (remove-if-not (lambda (v) (string-match package-version-format v))
                 (split-string (shell-command-to-string "git tag")
                               "\n" t)))

(defun package-public-file-candidates (name version)
  "Return a list of possible names for the specified package.

Use `package-file-types' to build a list of potential file
names for a package named NAME with version VERSION."
  (mapcar
   '(lambda (type)
      (format "%s/%s-%s.%s" package-public-dir name version (cdr type)))
   package-file-types))

(defun* package-file-exists (candidates)
  "Finds which (if any) of the possible file names exists.

CANDIDATES is a list of file names which might exist. They will
be checked, and if one of the specified files does exist, its
name will be returned."
  (dolist (file candidates)
    (when (file-exists-p file)
      (return-from package-file-exists file)))
  nil)

(defun package-built? (name version)
  "Check whether there is a package file matching NAME and VERSION."
  (package-file-exists (package-public-file-candidates name version)))

(defun package-build-archive-contents (projects)
  "Update the list of packages.

PROJECTS is a list of package descriptions."
  (let ((print-level nil)
        (print-length nil)
        (contents (package-get-archive-contents projects)))
    (write-region (concat (pp-to-string contents) "\n") nil
                  (concat package-public-dir
                          "/archive-contents"))))

(defun package-get-archive-contents (projects)
  "Build the package index array for PROJECTS."
  (cons package-archive-version
        (remove-if
         'null (mapcar 'package-archive-contents-for-project projects))))

(defun* package-get-type (file)
  "Returns the package type of the given FILE.

Checks FILE's extension against `package-file-types'."
  (dolist (type package-file-types)
    (when (string-match (concat "\\." (cdr type) "$") file)
      (return-from package-get-type (car type))))
  nil)

(defun package-archive-contents-for-project (project)
  "Build a package structure for the latest version of PROJECT."
  (let ((pkg-file (package-file-exists (package-latest-for-project project))))
    (when pkg-file
      (let* ((type (package-get-type pkg-file))
             (info (cond
                   ((eq type 'single)
                    (find-file pkg-file)
                    (package-buffer-info))
                   ((eq type 'tar)
                    (package-tar-file-info pkg-file)))))
        (cons (intern (project-name project))
              (make-pkg-info :version (package-version-split (pkg-buf-info-version info))
                             :requires (pkg-buf-info-requires info)
                             :desc (if (string= (pkg-buf-info-desc info) "")
                                       (read-string "Description of package: ")
                                     (pkg-buf-info-desc info))
                             :type type))))))

(defun package-latest-for-project (project)
  "Return a list of the latest candidate files for PROJECT.

Calls `package-public-file-candidates' to get a list of candidate
files of the most recent version of PROJECT."
  (cd (package-local-checkout-dir (project-name project)))
  (let* ((versions (package-list-versions))
         (latest-version (car (last (package-sort-versions versions)))))
    (package-public-file-candidates (project-name project) latest-version)))

(defun package-sort-versions (versions)
  "Sort the list of VERSIONS using `package-version-compare'."
  ;; destructive list functions! you gotta be kidding me.
  (let ((versions (copy-list versions)))
    (sort versions (lambda (a b)
                     (package-version-compare
                      (package-version-split a)
                      (package-version-split b) '<)))))

(provide 'package-maint)

;;; package-maint.el ends here
