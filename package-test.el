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
                      :type 'single
                      :status 'obsolete))
          (test-pkg2 (cl-merge-struct 'package
                                      (copy-package test-pkg1)
                                      (make-package
                                       :version '(1 1)
                                       :authors '(("Joe Bob" . "jbob@example.com")
                                                  ("Fred Jones" . "fjones@example.com"))
                                       :version-raw "1.1"
                                       :created "10 Mar 2008"
                                       :updated "10 Mar 2008"
                                       :status 'activated)))
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
                                     :wikipage "deppy.el"
                                     :status 'available)))
          (tarty (cl-merge-struct 'package
                                  (let ((tmp (copy-package test-pkg1)))
                                    (setf (package-requires-hard tmp) nil)
                                    tmp)
                                  (make-package
                                   :name 'tarty
                                   :version '(1 5 -3 3)
                                   :version-raw "1.5alpha3"
                                   :authors '(("George Tarterson" . "jtart@example.com"))
                                   :maintainer '("George Tarterson" . "jtart@example.com")
                                   :provides '(tarty)
                                   :homepage "tarty.example.com"
                                   :wikipage "tarty.el"
                                   :type 'tar
                                   :archive 'manual
                                   :status 'activated)
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
                                          :type 'builtin
                                          :status 'activated)
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
  (expect (package '((dep-pkg deppy)))
    (package-requires-hard test-pkg1))

  (desc "package-registry-flat")
  (expect (package (list test-pkg1
                         test-pkg2
                         dep-pkg
                         tarty
                         internal-pkg))
    (package-registry-flat))

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
  (expect (package (list dep-pkg))
    (package-required-packages test-pkg2))
  (expect (package nil)
    (package-required-packages dep-pkg))

  (desc "package-find")
  (expect (package (list test-pkg1 test-pkg2))
    (package-find 'test-pkg))
  (expect (package (list dep-pkg))
    (package-find 'dep-pkg))
  (expect (package (list test-pkg1))
    (package-find 'test-pkg :version '(1 0)))
  (expect (package (list test-pkg2))
    (package-find 'test-pkg :version '(1 1)))
  (expect (package (list test-pkg1))
    (package-find 'test-pkg :version-raw "1.0"))
  (expect (package (list internal-pkg))
    (package-find 'internal-pkg :maintainer '("RMS" . "rms@example.com")))
  (expect (package (list tarty))
    (package-find 'tarty :archive 'manual))
  (expect (package (list tarty))
    (package-find 'tarty :archive 'manual :type 'tar))
  (expect (package nil)
    (package-find 'tarty :archive 'manual :type 'single))
  (expect (package (list dep-pkg))
    (package-find 'dep-pkg :provides '(deppy)))

  (desc "package-find-latest")
  (expect (package test-pkg2)
    (package-find-latest 'test-pkg nil))
  (expect (error)
    (with-package-test
     (package-find-latest 'test-pkg nil :license "gpl2")))
  (expect (package nil)
    (package-find-latest 'test-pkg t :license "gpl2"))
  (expect (package nil)
    (package-find-latest 'dep-pkg t
                         :provides '(deppy)
                         :wikipage "not-dep.el"))
  (expect (package dep-pkg)
    (package-find-latest 'dep-pkg t
                         :provides '(deppy)
                         :wikipage "deppy.el"))

  (desc "package-compute-transaction")
  (expect (package (list test-pkg2))
    (package-compute-transaction (list test-pkg2) nil))
  (expect (package (list dep-pkg))
    (package-compute-transaction nil (package-required-packages test-pkg2)))
  (expect (package (list dep-pkg))
    (package-compute-transaction (list dep-pkg) nil))
  (expect (package (list dep-pkg test-pkg2))
    (package-compute-transaction (list test-pkg2) (package-required-packages test-pkg2)))
  (expect (package (list dep-pkg test-pkg2))
    (package-compute-transaction (list dep-pkg test-pkg2) (package-required-packages test-pkg2)))
  (expect (package (list dep-pkg))
    (package-compute-transaction nil (list dep-pkg)))
  (expect (package (list dep-pkg test-pkg2))
    (package-compute-transaction nil (list dep-pkg test-pkg2)))
  (expect (package (list dep-pkg test-pkg2))
    (package-compute-transaction nil (list test-pkg2 dep-pkg)))

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

  (desc "package-print-package")
  (expect (regexp "  test-pkg            1.0         obs     Simple package system for Emacs *")
          (with-output-to-string
            (with-package-test
             (package-print-package test-pkg1))))
  (expect (regexp "  test-pkg            1.1         act     Simple package system for Emacs *")
          (with-output-to-string
            (with-package-test
             (package-print-package test-pkg2))))
  (expect (regexp "  test-pkg            1.1         act     Simple package system for Emacs *")
          (with-output-to-string
            (with-package-test
             (package-print-package test-pkg2))))
  (expect (regexp "  test-pkg            1.1         act     Simple package system for Emacs *\n")
          (with-output-to-string
            (with-package-test
             (package-print-package test-pkg2 t))))

  (expect "test-pkg            act     1.1           \n"
          (with-output-to-string
            (with-package-test
             (let ((package-menu-columns (list package-menu-column-name
                                               package-menu-column-status
                                               package-menu-column-version
                                               package-menu-column-command)))
               (package-print-package test-pkg2 t)))))

  (desc "package-menu-parse-line")
  (expect '(command ""
                     name "test-pkg"
                     version "1.1"
                     status "act"
                     summary "Simple package system for Emacs")
    (with-temp-buffer
      (insert "  test-pkg            1.1         act     Simple package system for Emacs\n")
      (package-menu-parse-line nil (point-min))))
  (expect '(command ""
                     name "test-pkg"
                     version "1.1"
                     status "act"
                     summary "Simple package system for Emacs")
    (with-temp-buffer
      (insert "  test-pkg            1.1         act     Simple package system for Emacs")
      (package-menu-parse-line nil (point-min))))
  (expect '(command "I"
                     name "test-pkg"
                     version "1.1"
                     status "act"
                     summary "Simple package system for Emacs")
    (with-temp-buffer
      (insert "I test-pkg            1.1         act     Simple package system for Emacs\n")
      (package-menu-parse-line nil (point-min))))
  (expect 75
          (with-temp-buffer
            (insert "I test-pkg            1.1         act     Simple package system for Emacs\n")
            (package-menu-parse-line nil (point-min))
            (point)))

  (desc "package-menu-make-pkg")
  (expect (make-package :name 'test-pkg
                        :version '(1 1)
                        :status 'activated
                        :summary "Simple package system for Emacs")
    (package-menu-make-pkg '(command "I"
                                      name "test-pkg"
                                      version "1.1"
                                      status "act"
                                      summary "Simple package system for Emacs")))
  (expect (make-package :name 'builtin-pkg
                        :version '(2 1 -3 2)
                        :status 'installed
                        :summary "Simple package system for Emacs")
    (package-menu-make-pkg '(command "I"
                                      name "builtin-pkg"
                                      version "2.1alpha2"
                                      status "inst"
                                      summary "Simple package system for Emacs")))

  (desc "package-menu-get-command")
  (expect 'package-install
    (package-menu-get-command '(command "I"
                                         name "builtin-pkg"
                                         version "2.1alpha2"
                                         status "inst"
                                         summary "Simple package system for Emacs")))
  (expect 'package-delete
    (package-menu-get-command '(command "D"
                                         name "builtin-pkg"
                                         version "2.1alpha2"
                                         status "inst"
                                         summary "Simple package system for Emacs")))
  (expect nil
          (package-menu-get-command '(command ""
                                              name "builtin-pkg"
                                              version "2.1alpha2"
                                              status "inst"
                                              summary "Simple package system for Emacs")))
  (expect nil
          (package-menu-get-command '(command " "
                                              name "builtin-pkg"
                                              version "2.1alpha2"
                                              status "inst"
                                              summary "Simple package system for Emacs")))

  (desc "package-list-packages-internal")
  (expect (regexp "  dep-pkg             2.0         avail   Simple package system for Emacs *
  internal-pkg        2.0beta2    act     Simple package system for Emacs *
  tarty               1.5alpha3   act     Simple package system for Emacs *
  test-pkg            1.0         obs     Simple package system for Emacs *
  test-pkg            1.1         act     Simple package system for Emacs *\n")
          (with-package-test
           (with-output-to-string
             (with-temp-buffer

               (package-list-packages-internal (current-buffer))
               (buffer-string)))))
  ;; Cheat and use `package-print-package' to simplify.
  (expect (package (mapconcat '(lambda (item) (with-output-to-string (package-print-package item t)))
                              (list test-pkg1
                                    test-pkg2
                                    tarty
                                    internal-pkg
                                    dep-pkg) ""))
    (with-output-to-string
     (with-temp-buffer
       (package-list-packages-internal (current-buffer) 'version)
       (buffer-substring (point-min) (point-max)))))

  (desc "package-menu-compute-header-line")
  (expect " Package Version Status Summary "
          (package-menu-compute-header-line))
  (expect '(space :align-to 2)
          (get-text-property 0 'display (package-menu-compute-header-line)))
  (expect (make-package-menu-col :name "Package"
                                 :type 'name
                                 :width 20
                                 :reader 'intern
                                 :writer 'package-name
                                 :comparator 'string-lessp)
          (get-text-property 1 'package-menu-col (package-menu-compute-header-line)))
  (expect '(space :align-to 22)
          (get-text-property 8 'display (package-menu-compute-header-line)))
  (expect (make-package-menu-col :name "Version"
                                 :type 'version
                                 :width 12
                                 :reader 'version-to-list
                                 :writer 'package-version-canonical
                                 :comparator 'version-list-<)
          (get-text-property 9 'package-menu-col (package-menu-compute-header-line)))
  (expect '(space :align-to 34)
          (get-text-property 16 'display (package-menu-compute-header-line)))
  (expect (make-package-menu-col :name "Status"
                                 :type 'status
                                 :width 8
                                 :reader 'package-status-symbol
                                 :writer 'package-status-string
                                 :comparator 'string-lessp)
          (get-text-property 17 'package-menu-col (package-menu-compute-header-line)))
  (expect '(space :align-to 42)
          (get-text-property 23 'display (package-menu-compute-header-line)))
  (expect (make-package-menu-col :name "Summary"
                                 :type 'summary
                                 :width 60
                                 :reader 'identity
                                 :writer 'package-summary
                                 :comparator 'string-lessp)
          (get-text-property 24 'package-menu-col (package-menu-compute-header-line)))

  (desc "package-install")
  (expect (package 'completed)
          (mocklet (((package-download-transaction (list tarty))))
                   (package-install (make-package :name 'tarty))
                   'completed))
  (expect (package 'completed)
          (mocklet (((package-download-transaction (list tarty))))
                   (package-install (make-package :name 'tarty :version '(1 5 -3 3)))
                   'completed))
  (expect (package '(mock-error not-called))
          (condition-case err
              (mocklet (((package-download-transaction (list tarty))))
                       (package-install (make-package :name 'not-found))
                       'completed)
            (error err)))

  (desc "package-menu-column-offset")
  (expect 0
          (package-menu-column-offset package-menu-column-command))
  (expect 2
          (package-menu-column-offset package-menu-column-name))
  (expect 22
          (package-menu-column-offset package-menu-column-version))
  (expect 0
          (let ((package-menu-columns (list package-menu-column-name
                                            package-menu-column-command)))
            (package-menu-column-offset package-menu-column-name)))
  (expect 20
          (let ((package-menu-columns (list package-menu-column-name
                                            package-menu-column-command)))
            (package-menu-column-offset package-menu-column-command)))

  (desc "package-menu-mark-internal")
  (expect 'package-delete
          (with-temp-buffer
            (insert (with-output-to-string
                     (package-print-package (make-package :name 'irrelevant :version '(1 2 3)))))
            (package-menu-mark-internal "D" (point-min))
            (package-menu-get-command (package-menu-parse-line nil (point-min)))))
  (expect 'package-install
          (with-temp-buffer
            (insert (with-output-to-string
                     (package-print-package (make-package :name 'irrelevant :version '(1 2 3)))))
            (package-menu-mark-internal "I" (point-min))
            (package-menu-get-command (package-menu-parse-line nil (point-min)))))
  (expect nil
          (with-temp-buffer
            (insert (with-output-to-string
                     (package-print-package (make-package :name 'irrelevant :version '(1 2 3)))))
            (package-menu-get-command (package-menu-parse-line nil (point-min)))))
  (expect 'package-install
          (let ((package-menu-columns (list package-menu-column-name
                                            package-menu-column-version
                                            package-menu-column-status
                                            package-menu-column-command)))
            (with-temp-buffer
            (insert (with-output-to-string
                      (package-print-package (make-package :name 'irrelevant :version '(1 2 3)) t)))
            (package-menu-mark-internal "I" (point-min))
            (package-menu-get-command (package-menu-parse-line nil (point-min))))))

  (desc "package-menu-execute")
  (expect (package 'completed)
          (with-temp-buffer
            (condition-case err
                (mocklet (((package-install *)))
                         (loop for pkg in (list test-pkg1 dep-pkg)
                               do (progn (insert (with-output-to-string
                                                   (package-print-package pkg t)))
                                         (package-menu-mark-internal "I" (line-beginning-position -1))))
                         (package-menu-execute)
                         'completed)
              (error err))))

  (desc "package-find-rest")
  (expect (package test-pkg1)
          (package-find-rest (make-package :name 'test-pkg :version '(1 0))))
  (expect (package test-pkg2)
          (package-find-rest (make-package :name 'test-pkg :version '(1 1))))
  (expect (error error "Expected only a single matching package, 2 found")
          (with-package-test
           (package-find-rest (make-package :name 'test-pkg))))
  (expect (package nil)
          (package-find-rest (make-package :name 'test-pkg) t))
  (expect (package tarty)
          (package-find-rest (make-package :name 'tarty) t))

  (desc "package-menu-view-commentary")
  (expect (package "Package information for test-pkg\n\nThis is a completely great testing package")
          (let (wind)
            (with-temp-buffer
              (insert (with-output-to-string
                        (package-print-package test-pkg1)))

              (setq wind (package-menu-view-commentary)))
            (with-current-buffer (window-buffer wind)
              (buffer-string))))
  (expect (package t)
          (let (wind)
            (with-temp-buffer
              (insert (with-output-to-string
                        (package-print-package test-pkg1)))

              (setq wind (package-menu-view-commentary)))
            (with-current-buffer (window-buffer wind)
              buffer-read-only)))
  (expect (package nil)
          (let (wind)
            (with-temp-buffer
              (insert (with-output-to-string
                        (package-print-package test-pkg1)))

              (setq wind (package-menu-view-commentary)))
            (with-current-buffer (window-buffer wind)
              (buffer-modified-p))))
  )

(provide 'package-test)

;;; package-test.el ends here
