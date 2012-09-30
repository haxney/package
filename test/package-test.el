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

;; Highlight `ert-deftest' declaration
(eval-after-load "semantic-el"
  '(semantic-elisp-setup-form-parser
       (lambda (form start end)
         (semantic-tag-new-function
          (symbol-name (nth 1 form))
          nil
          (semantic-elisp-desymbolify-args (nth 2 form))
          :user-visible-flag (eq (car-safe (nth 4 form)) 'interactive)
          :documentation (semantic-elisp-do-doc (nth 3 form))))
     ert-deftest))

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
  "Expected `package-desc' parsed from simple-single.el.")

(defmacro with-package-test (&rest body)
  "Set up temporary locations and variables for testing."
  `(let ((package-user-dir package-test-user-dir))
     ,@body
     (when (file-directory-p package-test-user-dir)
       (delete-directory package-test-user-dir t))))

(ert-deftest package-test-buffer-info ()
  "Parse an elisp buffer to get a `package-desc' object."
  (with-temp-buffer
    (insert-file-contents "simple-single.el")
    (should (equal (package-buffer-info) simple-single-desc))))

(ert-deftest package-test-install-single ()
  "Install a single file without using an archive."
  (with-temp-buffer
    (insert-file-contents "simple-single.el")
    (should (eq (package-install-from-buffer (package-buffer-info)) t))))

(provide 'package-test)

;;; package-test.el ends here