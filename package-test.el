;;; package-test.el --- Test suite for package.el

;; Copyright (C) 2010 Daniel Hackney

;; Author: Daniel Hackney

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
                                   :type 'tar
                                   :archive 'manual)
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
          tarty-file
          (simple-file ";;; simple-file.el --- A simple Elisp file for testing.

;; Copyright (C) 2010 Example Ample

;; Author: Example Ample <ample@example.com>
;; Created: 5 Jan 2010
;; Version: 1.2.3
;; Keywords: tools

;;; Commentary:

;; This is a cool simple file which doesn't actually do stuff.

;;; Code:

\(provide 'simple-file\)

;;; simple-file.el ends here
")
          (simple-file-pkg (make-package :version '(1 2 3)
                                         :version-raw "1.2.3"
                                         :summary "A simple Elisp file for testing"
                                         :created "20100105"
                                         :updated "2010"
                                         :authors '(("Example Ample" . "ample@example.com"))
                                         :maintainer '("Example Ample" . "ample@example.com")
                                         :provides '(simple-file)
                                         :keywords '("tools")
                                         :commentary "This is a cool simple file which doesn't actually do stuff.\n"
                                         :type 'single
                                         :archive 'manual))
          (package-registry
           `((test-pkg . (,test-pkg1 ,test-pkg2))
             (dep-pkg . (,dep-pkg))
             (tarty . (,tarty))
             (internal-pkg . (,internal-pkg))))
          (test-dir (file-name-as-directory (make-temp-name (expand-file-name "package-test"
                                                                              temporary-file-directory))))
          test-dir-created
          (package-archives `((manual ,(concat "file://" test-dir) ,test-dir))))
     (flet ((make-tar (base files)
                      (let* ((base-abs (expand-file-name base test-dir))
                             (file-list
                              (loop for f in files
                                    for name = (car f)
                                    for full-path = (expand-file-name name base-abs)
                                    for contain-dir = (file-name-directory full-path)
                                    for contents = (cdr f)
                                    do (progn
                                         (make-directory contain-dir t)
                                         (with-temp-file full-path
                                           (insert contents)))))
                             (output-abs (concat base-abs ".tar")))
                        (shell-command (format "tar -cf %s -C %s %s"
                                               output-abs
                                               test-dir
                                               base))
                        output-abs))
            ;; Setup various parts of the test, such as creating files and
            ;; directories and such.
            (setup-test (&rest options)
                        (dolist (op options)
                          (case op
                            (basic (setf (package-requires-hard dep-pkg) nil))
                            (test-dir (make-directory test-dir t)
                                      (setq test-dir-created t))
                            (tarty
                             (setq tarty-file
                                   (make-tar "tarty-1.5alpha3"
                                             `(("file1.el" . ";;; file1.el --- This is file 1")
                                               ("file2.el" . ";;; file2.el --- This is file 2")
                                               ("info.epkg" . ,(cl-merge-pp tarty 'package))))))))))
       (prog1
           (progn
             (setup-test 'basic)
             ,@body)
         (when test-dir-created
           (require 'dired)
           (dired-delete-file test-dir 'always))))
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
    (caar package-registry))
  (expect (package test-pkg1)
    (cadar package-registry))

  (desc "package-split-filename")
  (expect (package '(package . (0 1 1)))
    (package-split-filename "package-0.1.1"))
  (expect (package '(package . (0 1 1)))
    (package-split-filename "package-0.1.1/"))
  (expect (package '(package . (0 1 1)))
    (package-split-filename (concat test-dir "package-0.1.1")))
  (expect (package '(package . (0 1 1)))
    (package-split-filename (concat test-dir "package-0.1.1/")))

  (expect (package '(package-test . (0 2 3)))
    (package-split-filename "package-test-0.2.3"))
  (expect (package '(package-test . (0 2 3)))
    (package-split-filename "package-test-0.2.3/"))
  (expect (package '(package-test . (0 2 3)))
    (package-split-filename "/package-test-0.2.3"))

  (expect '(tar-test . (1 -1 2))
    (package-split-filename "tar-test-1rc2.tar" "tar"))
  (expect (package '(tar-test . (1 -1 2)))
    (package-split-filename (concat test-dir "tar-test-1rc2.tar") "tar"))

  (expect (package '(package-test . (0 2 3)))
    (package-split-filename (concat test-dir "package-test-0.2.3/")))
  (expect (package '(package-test . (0 2 3)))
    (package-split-filename (concat test-dir "package-test-0.2.3")))
  (expect (package '(package-test . (0 2 3)))
    (package-split-filename (concat test-dir "package-test-0.2.3/")))

  (expect (error error *)
    (package-split-filename "package-test-0.1.this-is-a-bad-name_#-" "" nil))
  (expect nil
    (package-split-filename "package-test-0.1.this-is-a-bad-name_#-" "" t))

  (desc "package-required-packages")
  (expect (package (list dep-pkg))
    (package-required-packages test-pkg1))
  (expect (package nil)
    (package-required-packages dep-pkg))

  (desc "package-find")
  (expect (package (list test-pkg1 test-pkg2))
    (package-find 'test-pkg))

  ;; Re-enable this later
  (desc "package-compute-transaction")
  ;; (expect (package (package (list test-pkg2 dep-pkg)))
  ;;   (package-compute-transaction (list test-pkg2) (package-required-packages test-pkg2)))

  (desc "package-info-file")
  (expect (package (concat test-dir "package-test-1.2.3/info.epkg"))
    (package-info-file (make-package :name 'package-test
                                     :version '(1 2 3)
                                     :archive 'manual)))

  (expect (package "package-test-1.2.3/info.epkg")
    (package-info-file (make-package :name 'package-test
                                     :version '(1 2 3)
                                     :archive 'manual) t))

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
  (expect (package 'single)
    (package-type-from-filename (concat test-dir "name.el")))
  (expect 'single
    (package-type-from-filename "name.el"))
  (expect (package 'single)
    (package-type-from-filename (concat test-dir "name.el") t))
  (expect (package 'tar)
    (package-type-from-filename (concat test-dir "name.tar")))
  (expect (package 'tar)
    (package-type-from-filename (concat test-dir "name.tar") t))
  (expect (error error (format "Could not find package type for extension: %s" "dog"))
    (with-package-test
     (package-type-from-filename (concat test-dir "name.dog"))))
  (expect (package nil)
    (package-type-from-filename (concat test-dir "name.dog") t))
  (expect (package nil)
    (package-type-from-filename (concat test-dir "package-test-0.2.3") t))

  (desc "package-from-filename")
  (expect (package (make-package :name 'package-test
                                 :version '(0 2 3)
                                 :archive 'manual
                                 :type 'tar))
    (package-from-filename (concat test-dir "package-test-0.2.3.tar")))
  (expect (package (make-package :name 'package-test
                                 :version '(0 2 3)
                                 :archive 'manual))
    (package-from-filename (concat test-dir "package-test-0.2.3") nil t))

  (desc "package-type-from-buffer")
  (expect 'single
    (with-temp-buffer
      (insert ";;; empty.el --- An empty file for testing")
      (package-type-from-buffer (current-buffer))))
  (expect (package 'tar)
    (setup-test 'test-dir 'tarty)
    (with-temp-buffer
      (insert-file-contents-literally tarty-file)
      (package-type-from-buffer (current-buffer))))

  (desc "make-tar")
  (expect (package (concat test-dir "out.tar"))
    (setup-test 'test-dir)
    (make-tar "out" '(("name" . "contents"))))
  (expect (package '("out/" "out/name"))
    (setup-test 'test-dir 'tarty)
    (with-temp-buffer
      (insert-file-contents-literally (make-tar "out" '(("name" . "contents"))))
      (mapcar 'tar-header-name (package-tar-items (current-buffer)))))
  (expect (package '("contents"))
    (setup-test 'test-dir 'tarty)
    (with-temp-buffer
      (insert-file-contents-literally (make-tar "out" '(("name" . "contents"))))
      (delete ""
              (mapcar 'package-tar-item-contents (package-tar-items (current-buffer))))))

  (desc "package-from-tar-buffer")
  (expect (package tarty)
    (setup-test 'test-dir 'tarty)
    (with-temp-buffer
      (insert-file-contents-literally tarty-file)
      (package-from-tar-buffer (current-buffer))))

  (desc "package-from-buffer")
  (expect (package tarty)
    (setup-test 'test-dir 'tarty)
    (with-temp-buffer
      (insert-file-contents-literally tarty-file)
      (package-from-buffer (current-buffer))))

  (desc "package-from-file")
  (expect (package simple-file-pkg)
    (setup-test 'test-dir)
    (let ((file (make-temp-file test-dir nil ".el")))
      (with-temp-file file
        (insert simple-file))
      (package-from-file file)))
  (expect (package tarty)
    (setup-test 'test-dir 'tarty)
    (package-from-file tarty-file))

  (desc "package-delete")
  (expect (package nil)
    (setup-test 'test-dir 'tarty)
    (package-delete tarty)
    (file-directory-p (package-install-directory tarty)))
  )

(provide 'package-test)

;;; package-test.el ends here
