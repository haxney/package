;;; package-maint.el --- Tools for curating the package archive

;; Copyright (C) 2009 Phil Hagelberg <technomancy@gmail.com>

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Created: 2 Jan 2009
;; Version: 0.9
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

;; Currently only supports single-file projects stored in git.

;;; Code:

;; Since this library is not meant to be loaded by users
;; at runtime, use of cl functions should not be a problem.
(require 'cl)                                        

(require 'package)

(defvar package-index "~/src/package.el/index.el"
  "The listing of all projects and repositories to get them from.
Should contain an alist of project names (symbols) to DVCS
repositories (strings).")

(defvar package-working-dir "~/src/package.el/working/%s")

(defvar package-public-dir "~/src/package.el/public"
  "Directory in which to place packages created.")

(defvar package-version-format "^v\\([0-9\\.]+[0-9]\\)*$"
  "A regex that will only match tags which indicate versions.")

(defun package-build-archive ()
  "Build packages for every version of every project in the index."
  (interactive)
  (save-excursion
    (find-file package-index)
    (let ((projects (package-read-from-string
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))))
      (dolist (project projects)
        (package-build-packages project))
      (package-build-archive-contents projects))))

(defun package-build-packages (project)
  "Given a project, create packages for each version that exists."
  (let ((name (car project))
        (dir default-directory))
    (package-init project)
    (cd (package-local-checkout name))
    (shell-command "git pull --tags")
    (dolist (version (package-list-versions))
      (when (not (package-built? name version))
        (package-build-package name version)))
    (cd dir)))

(defun package-build-package (name version)
  "Given a project version, create a package for it."
  (shell-command (format "git checkout v%s" version))
  (find-file (format "%s/%s.el" (package-local-checkout name) name))
  (package-write-buffer)
  (kill-buffer))

(defun package-write-buffer ()
  "Write a package whose contents are in the current buffer."
  (save-excursion
    (save-restriction
      (let* ((pkg-info (package-buffer-info))
             (pkg-version (aref pkg-info 3))
             (file-name (aref pkg-info 0)))
        (write-region (point-min) (point-max)
                      (concat package-archive-public-dir
                              file-name "-" pkg-version
                              ".el")
                      nil nil nil 'excl)
        ;; special-case "package": write a second copy so that the
        ;; installer can easily find the latest version.
        (if (string= file-name "package")
            (write-region (point-min) (point-max)
                          (concat package-archive-public-dir
                                  file-name ".el")
                          nil nil nil 'ask))))))

(defun package-init (project)
  "Create a new checkout of a project if necessary."
  (when (not (file-exists-p (package-local-checkout project)))
    (cd package-working-dir)
    (shell-command (format "git clone %s" (cadr project)))))

(defun package-local-checkout (name)
  (format package-working-dir name))

(defun package-list-versions ()
  "List all versions of a project. Must run in project checkout."
  (mapcar (lambda (v) (substring v 1))
          (remove-if-not (lambda (v) (string-match package-version-format v))
                         (split-string (shell-command-to-string "git tag")
                                       "\n" t))))

(defun package-directory (name version)
  (format "%s/%s-%s" package-public-dir name version))

(defun package-built? (name version)
  (file-exists-p (package-directory name version)))

(defun package-build-archive-contents (contents)
  "Update the list of packages."
  (let ((print-level nil)
        (print-length nil))
    (write-region (concat (pp-to-string contents) "\n") nil
                  (concat package-public-dir
                          "archive-contents"))))

(defun package-get-archive-contents (projects)
  (cons package-archive-version
        (mapcar 'package-contents-for-project projects)))

(defun package-contents-for-project (project)
  (find-file (package-latest-for-project project))
  (let* ((pkg-info (package-buffer-info))
         (pkg-version (aref pkg-info 3))
         (split-version (package-version-split pkg-version))
         (requires (aref pkg-info 1))
         (desc (if (string= (aref pkg-info 2) "")
                   (read-string "Description of package: ")
                 (aref pkg-info 2)))
         ;; TODO: support tar
         (file-type 'single))
    (vector split-version requires desc file-type)))

(defun package-latest-for-project (project)
  ;; TODO: write
  )

(provide 'package-maint)
;;; package-maint.el ends here
