;;; package-test.el --- Test suite for package.el

;; Copyright (C) 2010 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: convenience abbrev local matching

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Testing for package.el using the el-expectations library.

;;; Code:
(require 'el-expectations)
(require 'package)

(defmacro with-package-test (&rest body)
  "Set up environment for testing with package"
  `(let* ((test-pkg1 (make-package
                      :name 'test-pkg
                      :version '(1 0)
                      :version-raw "1.0"
                      :summary "Simple package system for Emacs"
                      :created "10 Mar 2007"
                      :updated "10 Mar 2007"
                      :license "gpl3"
                      :authors '(("Joe Bob" . "jbob@example.com"))
                      :maintainer '("Joe Bob" . "jbob@example.com")
                      :provides '(test-pkg)
                      :requires-hard '((dep-pkg deppy))
                      :requires-soft '()
                      :keywords '("tools" "libraries")
                      :homepage "www.example.com"
                      :wikipage "test-pkg.el"
                      :commentary "This is a completely great testing package"
                      :archive 'elpa
                      :type 'single))
          (test-pkg2 (cl-merge-struct 'package
                                      (copy-package test-pkg1)
                                      (make-package
                                       :version '(1 1)
                                       :version-raw "1.1"
                                       :created "10 Mar 2008"
                                       :updated "10 Mar 2008")))
          (dep-pkg (cl-merge-struct 'package
                                    (copy-package test-pkg1)
                                    (make-package
                                     :name 'dep-pkg
                                     :version '(2 0)
                                     :version-raw "2.0"
                                     :authors '(("Sally Smith" . "ssmith@example.com"))
                                     :maintainer '("Sally Smith" . "ssmith@example.com")
                                     :requires-hard '(())
                                     :provides '(deppy)
                                     :homepage "deppy.example.com"
                                     :wikipage "deppy.el")
                                    ))
          (tarty (cl-merge-struct 'package
                                  (copy-package test-pkg1)
                                  (make-package
                                   :name 'tarty
                                   :version '(1 5 -3 3)
                                   :version-raw "1.5alpha3"
                                   :authors '(("George Tarterson" . "jtart@example.com"))
                                   :maintainer '("George Tarterson" . "jtart@example.com")
                                   :requires-hard '(())
                                   :provides '(tarty)
                                   :homepage "tarty.example.com"
                                   :wikipage "tarty.el"
                                   :type 'tar)
                                  ))
          (internal-pkg (cl-merge-struct 'package
                                  (copy-package test-pkg1)
                                  (make-package
                                   :name 'internal-pkg
                                   :version '(2 0 -2 2)
                                   :version-raw "2.0beta2"
                                   :authors '(("RMS" . "rms@example.com"))
                                   :maintainer '("RMS" . "rms@example.com")
                                   :requires-hard '(())
                                   :provides '(internal-pkg)
                                   :homepage "internal.example.com"
                                   :wikipage "internal-pkg.el"
                                   :type 'builtin)
                                  ))
          (package-available-alist
           `((test-pkg . (,test-pkg1 ,test-pkg2))
             (dep-pkg . (,dep-pkg))
             (tarty . (,tarty))
             (internal-pkg . (,internal-pkg))))
          (test-tmp-dir "/tmp/package-test/")
          (package-archives `((test "file:///tmp/test/" ,test-tmp-dir)))
          )
     (prog2
         (make-directory test-tmp-dir t)
         (progn
          ,@body)
       (require 'dired)
       (dired-delete-file test-tmp-dir 'always))
     ))

(defun package-exps-assert-with-package-test (expected actual)
  (with-package-test
   (exps-do-assertion
    expected actual 'package t
    (lambda (e a) (equal (eval e) a))
    (lambda (e a) (format "FAIL: Expected <%S> but was <%S>" (eval e) a)))))

(add-to-list 'exps-assert-functions 'package-exps-assert-with-package-test)

(expectations
  (desc "Basic sanity tests")
  (expect (package 'test-pkg)
    (caar package-available-alist))
  (expect (package test-pkg1)
    (cadar package-available-alist))

  (desc "package-split-filename")
  (expect '(package . (0 1 1))
    (package-split-filename "package-0.1.1"))
  (expect '(package . (0 1 1))
    (package-split-filename "package-0.1.1/"))
  (expect '(package . (0 1 1))
    (package-split-filename "/tmp/package-test/package-0.1.1"))
  (expect '(package . (0 1 1))
    (package-split-filename "/tmp/package-test/package-0.1.1/"))

  (expect '(package-test . (0 2 3))
    (package-split-filename "package-test-0.2.3"))
  (expect '(package-test . (0 2 3))
    (package-split-filename "package-test-0.2.3/"))
  (expect '(package-test . (0 2 3))
    (package-split-filename "/package-test-0.2.3"))

  (expect '(tar-test . (1 -1 2))
    (package-split-filename "tar-test-1rc2.tar" "tar"))
  (expect '(tar-test . (1 -1 2))
    (package-split-filename "/tmp/packages/tar-test-1rc2.tar" "tar"))

  (expect '(package-test . (0 2 3))
    (package-split-filename "/tmp/package-test/package-test-0.2.3/"))
  (expect '(package-test . (0 2 3))
    (package-split-filename "/tmp/package-test/package-test-0.2.3"))
  (expect '(package-test . (0 2 3))
    (package-split-filename "/tmp/package-test/package-test-0.2.3/"))

  (expect (error error *)
    (package-split-filename "package-test-0.1.this-is-a-bad-name_#-" "" nil))
  (expect nil
    (package-split-filename "package-test-0.1.this-is-a-bad-name_#-" "" t))

  (desc "package-find")
  (expect (package (list test-pkg1 test-pkg2))
    (package-find 'test-pkg))

  (desc "package-compute-transaction")
  (expect (package (list test-pkg2 dep-pkg))
    (package-compute-transaction (list test-pkg2) (package-required-packages test-pkg2)))

  (desc "package-info-file")
  (expect (package (concat test-tmp-dir "package-test-1.2.3/info.epkg"))
    (package-info-file (make-package :name 'package-test
                                     :version '(1 2 3)
                                     :archive 'test)))

  (expect (package "package-test-1.2.3/info.epkg")
    (package-info-file (make-package :name 'package-test
                                     :version '(1 2 3)
                                     :archive 'test) t))

  (desc "package-suffix")
  (expect (package "el")
    (package-suffix test-pkg1))
  (expect (package "el")
    (package-suffix test-pkg2 t))
  (expect (package "tar")
    (package-suffix tarty))
  (expect (error error "Package is a builtin, and therefore does not have a suffix")
    (with-package-test
     (package-suffix internal-pkg)))
    (expect (package nil)
      (package-suffix internal-pkg t))

    (desc "package-type-from-filename")
    (expect 'single
      (package-type-from-filename "/tmp/pkg/name.el"))
    (expect 'single
      (package-type-from-filename "name.el"))
    (expect 'single
      (package-type-from-filename "/tmp/pkg/name.el" t))
    (expect 'tar
      (package-type-from-filename "/tmp/pkg/name.tar"))
    (expect 'tar
      (package-type-from-filename "/tmp/pkg/name.tar" t))
    (expect (error error (format "Could not find package type for extension: %s" "dog"))
      (package-type-from-filename "/tmp/pkg/name.dog"))
    (expect nil
      (package-type-from-filename "/tmp/pkg/name.dog" t))
    (expect nil
      (package-type-from-filename "/tmp/package-test/package-test-0.2.3" t))

    (desc "package-from-filename")
    (expect (package (make-package :name 'package-test
                                   :version '(0 2 3)
                                   :archive 'test
                                   :type 'tar))
      (package-from-filename "/tmp/package-test/package-test-0.2.3.tar"))
        (expect (package (make-package :name 'package-test
                                   :version '(0 2 3)
                                   :archive 'test))
          (package-from-filename "/tmp/package-test/package-test-0.2.3" nil t))

        (desc "package-type-from-buffer")
        (expect 'single
          (with-temp-buffer
            (insert ";;; empty.el --- An empty file for testing")
            (package-type-from-buffer (current-buffer))))

        (expect (package 'tar)
          (let ((file1 (make-temp-file test-tmp-dir nil ".el"))
                (file2 (make-temp-file test-tmp-dir nil ".el"))
                (info (concat test-tmp-dir "info.epkg"))
                (tar-file (make-temp-file "output" nil ".tar")))
            (with-temp-file file1
              (format ";;; %s --- This is file 1" file1))
            (with-temp-file file2
              (format ";;; %s --- This is file 2" file2))
            (with-temp-file info
              (cl-merge-pp (make-package :name 'read-tar
                                         :version '(0 1 2 3)
                                         :type 'tar) 'package))
            (shell-command (format "tar -cf %s %s %s %s"
                                   tar-file
                                   file1
                                   file2
                                   info))
            (with-temp-buffer
              (insert-file-contents-literally tar-file)
              (package-type-from-buffer (current-buffer)))))
  )

(provide 'package-test)

;;; package-test.el ends here
