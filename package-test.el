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
                      :requires-hard '((deppy deppy))
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
                                     :name 'deppy
                                     :version '(2 0)
                                     :version-raw "2.0"
                                     :authors '(("Sally Smith" . "ssmith@example.com"))
                                     :maintainer '("Sally Smith" . "ssmith@example.com")
                                     :requires-hard '(())
                                     :provides '(deppy)
                                     :homepage "deppy.example.com"
                                     :wikipage "deppy.el")
                                    ))
          (package-available-alist
           `((test-pkg . (,test-pkg1 ,test-pkg2))
             (dep-pkg . (,dep-pkg)))))
     ,@body))

(expectations
  (desc "Basic sanity test.")
  (expect 'test-pkg
    (with-package-test
     (caar package-archive-contents)))

  (desc "package-split-dirname")
  (expect '(package . (0 1 1))
    (package-split-dirname "package-0.1.1"))
  (expect '(package . (0 1 1))
    (package-split-dirname "package-0.1.1/"))
  (expect '(package . (0 1 1))
    (package-split-dirname "/tmp/package-test/package-0.1.1"))
  (expect '(package . (0 1 1))
    (package-split-dirname "/tmp/package-test/package-0.1.1/"))

  (desc "package-compute-transaction")
  (expect (test-pkg2 dep-pkg)
    (with-package-test
     (package-compute-transaction test-pkg2 (package-requires-hard test-pkg2))))
  )

(provide 'package-test)

;;; package-test.el ends here
