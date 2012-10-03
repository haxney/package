;;; package-test.el --- Tests for the Emacs package system

;; Author: Daniel Hackney <dan@haxney.org>
;; Version: 1.0

;; This file is not part of GNU Emacs.

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

;; Run this from a separate Emacs instance from your main one as it
;; messes with the installed packages. In fact, you should probably
;; back up your `package-user-dir' just in case!

;; Run this in a clean Emacs session using:
;;
;;     $ emacs -Q --batch -L . -l package-test.el -l ert -f ert-run-tests-batch-and-exit

;;; Code:

(require 'package)
(require 'ert)
(require 'cl-lib)

(defvar package-test-original-user-dir package-user-dir
  "Save the old value of `package-user-dir' to be restored later.")

(defvar package-test-user-dir (make-temp-name
                               (concat temporary-file-directory "pkg-test-user-dir-"))
  "Directory to use for installing packages during testing.")

(setq package-user-dir package-test-user-dir)

(defvar simple-single-desc [cl-struct-package-desc simple-single (1 3)
                                                   "A single-file package with no dependencies"
                                                   nil single nil (".")
                                                   ";;; Commentary:

;; This package provides a minor mode to frobnicate and/or bifurcate
;; any flanges you desire. To activate it, type \"C-M-r M-3 butterfly\"
;; and all your dreams will come true.

"]
  "Expected `package-desc' parsed from simple-single-1.3.el.")

(defvar package-test-dir (file-name-directory load-file-name)
  "Base directory of package test files.")

(defvar package-test-fake-contents-file
  (expand-file-name "archive-contents" package-test-dir)
  "Path to a static copy of \"archive-contents\".")

(defvar package-test-built-file-suffixes '(".tar" "/dir" "/*.info")
  "Remove these files when cleaning up a built package.")

(cl-defmacro with-package-test ((&optional &key file build-dir) &rest body)
  "Set up temporary locations and variables for testing."
  `(let ((package-user-dir package-test-user-dir)
         (package-archives `(("gnu" . ,package-test-dir)))
         ,@(if build-dir (list (list 'build-dir build-dir)
                                (list 'build-tar (concat build-dir ".tar")))
            (list 'none)))
     (unless (file-directory-p package-user-dir)
       (mkdir package-user-dir))
     ,(if build-dir
          (list 'package-test-build-multifile 'build-dir))
     (with-temp-buffer
       ,(if file
            (list 'insert-file-contents file))
       ,@body)
     ,(if build-dir
          (list 'package-test-cleanup-built-files build-dir))
     (when (file-directory-p package-test-user-dir)
       (delete-directory package-test-user-dir t))))

(defun package-test-install-texinfo (file)
  "Install from texinfo FILE.

FILE should be a .texinfo file relative to the current `default-directory'"
  (let* ((full-file (expand-file-name file))
         (info-file (replace-regexp-in-string "\\.texi\\'" ".info" full-file)))
    (with-current-buffer (find-file-literally full-file)
      (require 'makeinfo)
      (makeinfo-buffer)
      ;; Give `makeinfo-buffer' a chance to finish
      (while compilation-in-progress
        (sit-for 1))
      (kill-buffer)
      (call-process "ginstall-info" nil (get-buffer "*scratch*") nil
                    (format "--info-dir=%s" default-directory)
                    (format "%s" info-file)))))

(defun package-test-build-multifile (dir)
  "Build a tar package from a multiple-file directory DIR.

DIR must not have a trailing slash."
  (let* ((pkg-dirname (file-name-nondirectory dir))
         (pkg-name (package-strip-version pkg-dirname))
         (pkg-version (match-string-no-properties 2 pkg-dirname))
         (tar-name (concat pkg-dirname ".tar"))
         (default-directory (expand-file-name dir)))
    (package-test-install-texinfo (concat pkg-name ".texi"))
    (cd (concat default-directory "/.."))
    (call-process "tar" nil nil nil "-caf" tar-name pkg-dirname)))

(defun package-test-suffix-matches (base suffix-list)
  "Return file names matching BASE concatenated with each item in SUFFIX-LIST"
  (cl-mapcan
   '(lambda (item) (file-expand-wildcards (concat base item)))
   suffix-list))

(defun package-test-cleanup-built-files (dir)
  "Remove files which were the result of creating a tar archive.

DIR is the base name of the package directory, without the trailing slash"
  (let* ((pkg-dirname (file-name-nondirectory dir)))
    (dolist (file (package-test-suffix-matches dir package-test-built-file-suffixes))
      (delete-file file))))

(defun package-test-search-tar-file (filename)
  "Search the current buffer's `tar-parse-info' variable for FILENAME.

Must called from within a `tar-mode' buffer."
  (cl-dolist (header tar-parse-info)
             (let ((tar-name (tar-header-name header)))
              (when (string= tar-name filename)
                (cl-return t)))))

(ert-deftest package-test-buffer-info ()
  "Parse an elisp buffer to get a `package-desc' object."
  (with-package-test
   (:file "simple-single-1.3.el")
   (should (equal (package-buffer-info) simple-single-desc))))

(ert-deftest package-test-install-single ()
  "Install a single file without using an archive."
  (with-package-test
   (:file "simple-single-1.3.el")
   (should (eq (package-install-from-buffer (package-buffer-info)) t))
   (let ((simple-pkg-dir (file-name-as-directory
                          (expand-file-name
                           "simple-single-1.3"
                           package-test-user-dir))))
     (should (eq (file-directory-p simple-pkg-dir) t))
     (with-temp-buffer
       (insert-file-contents (expand-file-name "simple-single-pkg.el" simple-pkg-dir))
       (should (string= (buffer-string)
                        "(define-package \"simple-single\" \"1.3\" \"A single-file package with no dependencies\" nil)\n"))))))

(ert-deftest package-test-refresh-contents ()
  "Parse an \"archive-contents\" file."
  (with-package-test
   ()
   (package-refresh-contents)))

(ert-deftest package-test-install-single-from-archive ()
  "Install a single package from a package archive."
  (with-package-test
   ()
   (package-refresh-contents)
   (package-install 'simple-single)))

(ert-deftest package-test-build-multifile ()
  "Build a multi-file archive."
  (with-package-test
   (:build-dir "multi-file-0.2.3")
   (should (file-exists-p build-tar))
   (let ((suffixes
          (remove build-tar (package-test-suffix-matches
                             build-dir
                             package-test-built-file-suffixes))))
     (with-current-buffer (find-file build-tar)
      (dolist (file suffixes)
        (should (package-test-search-tar-file file)))
      (kill-buffer)))))

(ert-deftest package-test-update-listing ()
  "Ensure installed package status is updated."
  (with-package-test
   ()
   (package-list-packages)
   (search-forward-regexp "^ +simple-single")
   (package-menu-mark-install)
   (package-menu-execute)
   (should (package-installed-p 'simple-single))))

(provide 'package-test)

;;; package-test.el ends here
