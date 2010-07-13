;;; package-test.el --- Test suite for package.el

;; Copyright (C) 2010 Daniel Hackney

;; Author: Daniel Hackney <dan@haxney.org>

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
;; Testing for package.el using the el-expectations and el-mock libraries.

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
                      :provided '(test-pkg)
                      :required '(((dep-pkg deppy)) ())
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
                                     :provided '(deppy)
                                     :homepage "deppy.example.com"
                                     :wikipage "deppy.el"
                                     :status 'available)))
          (tarty (cl-merge-struct 'package
                                  (let ((tmp (copy-package test-pkg1)))
                                    (setf (package-required tmp) nil)
                                    tmp)
                                  (make-package
                                   :name 'tarty
                                   :version '(1 5 -3 3)
                                   :version-raw "1.5alpha3"
                                   :authors '(("George Tarterson" . "jtart@example.com"))
                                   :maintainer '("George Tarterson" . "jtart@example.com")
                                   :provided '(tarty)
                                   :homepage "tarty.example.com"
                                   :wikipage "tarty.el"
                                   :type 'tar
                                   :archive 'manual
                                   :status 'available)))
          (internal-pkg (cl-merge-struct 'package
                                         (copy-package test-pkg1)
                                         (make-package
                                          :name 'internal-pkg
                                          :version '(2 0 -2 2)
                                          :version-raw "2.0beta2"
                                          :authors '(("RMS" . "rms@example.com"))
                                          :maintainer '("RMS" . "rms@example.com")
                                          :required '(())
                                          :provided '(internal-pkg)
                                          :homepage "internal.example.com"
                                          :wikipage "internal-pkg.el"
                                          :type 'builtin
                                          :status 'activated)))
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
          (simple-file-pkg (make-package :name 'simple-file
                                         :version '(1 2 3)
                                         :version-raw "1.2.3"
                                         :summary "A simple Elisp file for testing"
                                         :created "20100105"
                                         :updated "2010"
                                         :authors '(("Example Ample" . "ample@example.com"))
                                         :maintainer '("Example Ample" . "ample@example.com")
                                         :provided '(simple-file)
                                         :keywords '("tools")
                                         :commentary "This is a cool simple file which doesn't actually do stuff.\n"
                                         :type 'single
                                         :archive 'manual))
          (package-registry
           `((test-pkg . (,test-pkg1 ,test-pkg2))
             (dep-pkg . (,dep-pkg))
             (tarty . (,tarty))
             (internal-pkg . (,internal-pkg))))
          (package-registry-alt `((dep-pkg ,dep-pkg)
                                  (test-pkg ,test-pkg1
                                            ,test-pkg2)
                                  (internal-pkg ,internal-pkg)
                                  (tarty ,tarty)))
          (test-dir (file-name-as-directory (make-temp-name (expand-file-name "package-test"
                                                                              temporary-file-directory))))
          test-dir-created
          (package-archives `((manual ,(concat "file://" test-dir "upstream/") ,test-dir)))
          (upstream-archive-contents "(2 (:name test-pkg
                   :version (1 0)
                   :version-raw \"1.0\"
                   :summary \"Simple package system for Emacs\"
                   :created \"10 Mar 2007\"
                   :updated \"10 Mar 2007\"
                   :license \"gpl3\"
                   :authors ((\"Joe Bob\" . \"jbob@example.com\"))
                   :maintainer (\"Joe Bob\" . \"jbob@example.com\")
                   :provided (test-pkg)
                   :required (((dep-pkg deppy)) ())
                   :keywords (\"tools\" \"libraries\")
                   :homepage \"www.example.com\"
                   :wikipage \"test-pkg.el\"
                   :commentary \"This is a completely great testing package\"
                   :archive elpa
                   :type single))"))
     (flet ((make-tar (base files)
                      (let* ((base-dir (concat test-dir "upstream/"))
                             (base-abs (expand-file-name base base-dir))
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
                             (output-abs (concat base-abs ".tar"))
                             (cmd (format "tar -cf %s -C %s %s"
                                               output-abs
                                               base-dir
                                               base)))
                        (shell-command cmd)
                        output-abs))
            ;; Setup various parts of the test, such as creating files and
            ;; directories and such.
            (setup-test (&rest options)
                        (dolist (op options)
                          (case op
                            (basic (setf (package-required dep-pkg) '(nil nil)))
                            (test-dir (make-directory (concat test-dir "upstream/") t)
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
           (dired-delete-file test-dir 'always))))))

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
    (package-required-hard test-pkg1))

  (desc "package-registry-flat")
  (expect (package (list test-pkg1
                         test-pkg2
                         dep-pkg
                         tarty
                         internal-pkg))
          (package-registry-flat))
  (expect (package (list dep-pkg
                         test-pkg1
                         test-pkg2
                         internal-pkg
                         tarty))
          (let ((package-registry package-registry-alt))
            (package-registry-flat)))

  (desc "package-split-filename")
  (expect (package '("package" . "0.1.1"))
    (package-split-filename "package-0.1.1"))
  (expect (package '("package" . "0.1.1"))
    (package-split-filename "package-0.1.1/"))
  (expect (package '("package" . "0.1.1"))
    (package-split-filename (concat test-dir "package-0.1.1")))
  (expect (package '("package" . "0.1.1"))
    (package-split-filename (concat test-dir "package-0.1.1/")))

  (expect (package '("package-test" . "0.2.3"))
    (package-split-filename "package-test-0.2.3"))
  (expect (package '("package-test" . "0.2.3"))
    (package-split-filename "package-test-0.2.3/"))
  (expect (package '("package-test" . "0.2.3"))
    (package-split-filename "/package-test-0.2.3"))

  (expect '("tar-test" . "1rc2")
    (package-split-filename "tar-test-1rc2.tar" "tar"))
  (expect (package '("tar-test" . "1rc2"))
    (package-split-filename (concat test-dir "tar-test-1rc2.tar") "tar"))

  (expect (package '("package-test" . "0.2.3"))
    (package-split-filename (concat test-dir "package-test-0.2.3/")))
  (expect (package '("package-test" . "0.2.3"))
    (package-split-filename (concat test-dir "package-test-0.2.3")))
  (expect (package '("package-test" . "0.2.3"))
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
          (package-find 'dep-pkg :provided '(deppy)))
  (expect (package (list dep-pkg))
          (let ((package-registry package-registry-alt))
            (package-find 'dep-pkg :provided '(deppy))))
  (expect (package (list tarty))
          (let ((package-registry package-registry-alt))
            (package-find 'tarty :archive 'manual :type 'tar)))

  (desc "package-find-latest")
  (expect (package test-pkg2)
    (package-find-latest 'test-pkg nil))
  (expect (package test-pkg2)
    (package-find-latest 'test-pkg t))
  (expect (package test-pkg2)
          (let ((package-registry `((test-pkg
                                     ,test-pkg2
                                     ,(make-package
                                       :name 'test-pkg
                                       :version '(0 9)
                                       :provided '(test-pkg)
                                       :archive 'elpa
                                       :type 'single
                                       :status 'obsolete)
                                     ,test-pkg1))))
            (package-find-latest 'test-pkg t)))
  (expect (package dep-pkg)
          (let ((package-registry `((dep-pkg
                                     ,(make-package
                                       :name 'dep-pkg
                                       :version '(0 9)
                                       :provided '(test-pkg)
                                       :archive 'elpa
                                       :type 'single
                                       :status 'obsolete)
                                     ,dep-pkg
                                     ,(make-package
                                       :name 'dep-pkg
                                       :version '(1 9)
                                       :provided '(test-pkg)
                                       :archive 'elpa
                                       :type 'single
                                       :status 'obsolete)))))
            (package-find-latest 'dep-pkg t)))
  (expect (error error "No package found named 'test-pkg' matching parameters '(:license gpl2)'")
    (with-package-test
     (package-find-latest 'test-pkg nil :license "gpl2")))
  (expect (package nil)
    (package-find-latest 'test-pkg t :license "gpl2"))
  (expect (package nil)
    (package-find-latest 'dep-pkg t
                         :provided '(deppy)
                         :wikipage "not-dep.el"))
  (expect (package dep-pkg)
    (package-find-latest 'dep-pkg t
                         :provided '(deppy)
                         :wikipage "deppy.el"))
  (expect (package nil)
          (package-find-latest 'dep-pkg t
                               :provided '(deppy)
                               :wikipage "deppy.el"
                               :version '(2 0)))
  (expect (error error "Version already specified; how do you expect me to find the latest?")
          (with-package-test
           (package-find-latest 'test-pkg nil
                                :license "gpl3"
                                :version '(1 2 3 4))))


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
                                 :version-raw "0.2.3"
                                 :archive 'manual
                                 :type 'tar))
    (package-from-filename (concat test-dir "package-test-0.2.3.tar")))
  (expect (package (make-package :name 'package-test
                                 :version '(0 2 3)
                                 :version-raw "0.2.3"
                                 :archive 'manual))
    (package-from-filename (concat test-dir "package-test-0.2.3") nil t))
  (expect (error error "Could not find package type for extension: 3")
      (with-package-test
       (package-from-filename (concat test-dir "package-test-0.2.3.thing") "thing")))
  (expect (package (make-package :name 'package-test
                                 :version '(0 2 3)
                                 :version-raw "0.2.3"
                                 :archive 'manual))
    (package-from-filename (concat test-dir "package-test-0.2.3.thing") "thing" t))
  (expect (make-package :name 'happy-pkg_ometer
                        :version '(0 2 3)
                        :version-raw "0.2.3"
                        :type 'single
                        :archive 'two)
    (let ((package-archives '((one "file:///path/to/one" "/path/to/one")
                              (two "file:///path/to/two" "/path/to/two")
                              (three "file:///path/to/three" "/path/to/three"))))
      (package-from-filename "/path/to/two/happy-pkg_ometer-0.2.3.el")))
  (expect (package (make-package :name 'simple-file
                                 :version '(1 2 3)
                                 :version-raw "1.2.3"
                                 :type 'single
                                 :archive 'manual))
    (let* ((dir (concat test-dir "simple-file-1.2.3/"))
           (file (concat dir "simple-file.el")))
      (package-from-filename file nil t)))

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
  (expect (package (concat test-dir "upstream/out.tar"))
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
    (let* ((dir (concat test-dir "simple-file-1.2.3/"))
           (file (concat dir "simple-file.el")))
      (make-directory dir)
      (with-temp-file file
        (insert simple-file))
      (package-from-file file)))
  (expect (package tarty)
    (setup-test 'test-dir 'tarty)
    (package-from-file tarty-file))

  (desc "package-install-directory")
  (expect (package (concat package-user-dir "package-1.0/"))
          (package-install-directory (make-package :name 'package
                                                   :archive 'elpa
                                                   :version '(1 0))))
  (expect (package "package-1.0/")
          (package-install-directory (make-package :name 'package
                                                   :archive 'elpa
                                                   :version '(1 0))
                                     t))

  (desc "package-delete")
  (expect (package nil)
    (setup-test 'test-dir 'tarty)
    (package-download tarty)
    (package-delete tarty)
    (file-directory-p (package-install-directory tarty)))
  (expect (package 'available)
    (setup-test 'test-dir 'tarty)
    (package-download tarty)
    (package-delete tarty)
    (package-status tarty))
  (expect (error error "Package test-pkg is not installed, so it cannot be deleted")
    (with-package-test
     (package-delete test-pkg1)))

  (desc "package-print-package")
  (expect (regexp "  test-pkg            1.0         obs     Simple package system for Emacs *")
          (with-output-to-string
            (with-package-test
             (package-print-package test-pkg1 nil standard-output))))
  (expect (regexp "  test-pkg            1.1         active  Simple package system for Emacs *")
          (with-output-to-string
            (with-package-test
             (package-print-package test-pkg2 nil standard-output))))
  (expect (regexp "  test-pkg            1.1         active  Simple package system for Emacs *")
          (with-output-to-string
            (with-package-test
             (package-print-package test-pkg2 nil standard-output))))
  (expect (regexp "  test-pkg            1.1         active  Simple package system for Emacs *\n")
          (with-output-to-string
            (with-package-test
             (package-print-package test-pkg2 t standard-output))))

  (expect "test-pkg            active  1.1           \n"
          (with-output-to-string
            (with-package-test
             (let ((package-menu-columns (list package-menu-column-name
                                               package-menu-column-status
                                               package-menu-column-version
                                               package-menu-column-command)))
               (package-print-package test-pkg2 t standard-output)))))

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
                                      status "active"
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
  internal-pkg        2.0beta2    active  Simple package system for Emacs *
  tarty               1.5alpha3   avail   Simple package system for Emacs *
  test-pkg            1.0         obs     Simple package system for Emacs *
  test-pkg            1.1         active  Simple package system for Emacs *\n")
          (with-package-test
           (with-temp-buffer
             (package-list-packages-internal (current-buffer))
             (buffer-string))))
  ;; Cheat and use `package-print-package' to simplify.
  (expect (package (mapconcat '(lambda (item) (with-output-to-string (package-print-package item t standard-output)))
                              (list test-pkg1
                                    test-pkg2
                                    tarty
                                    internal-pkg
                                    dep-pkg) ""))
          (with-temp-buffer
            (package-list-packages-internal (current-buffer) 'version)
            (buffer-string)))
  (expect (package (mapconcat '(lambda (item) (with-output-to-string (package-print-package item t standard-output)))
                              (list test-pkg1
                                    test-pkg2
                                    tarty
                                    internal-pkg
                                    dep-pkg) ""))
          (with-temp-buffer
            (package-list-packages-internal (current-buffer) package-menu-column-version)
            (buffer-string)))
  (expect (package (mapconcat '(lambda (item) (with-output-to-string (package-print-package item t standard-output)))
                              (list test-pkg1
                                    test-pkg2
                                    tarty
                                    internal-pkg
                                    dep-pkg
                                    ) ""))
          (with-temp-buffer
            (package-list-packages-internal (current-buffer) package-menu-column-version)
            (buffer-string)))

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
  (expect (package 'activated)
    (setup-test 'test-dir 'tarty)
    (package-install (make-package :name 'tarty)))
  (expect (package 'activated)
    (setup-test 'test-dir 'tarty)
    (package-install (make-package :name 'tarty :version '(1 5 -3 3))))
  (expect (not-called package-download-transaction)
    (condition-case err
        (with-package-test
         (package-install (make-package :name 'not-found)))
      (error err)))
  (expect (package 'activated)
    (setup-test 'test-dir 'tarty)
    (package-download tarty)
    (package-install tarty)
    (package-status tarty))
  (expect (package 'activated)
    (setup-test 'test-dir 'tarty)
    (package-download tarty)
    (package-install tarty)
    (package-status (car (package-find 'tarty))))

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
            (package-print-package (make-package :name 'irrelevant :version '(1 2 3)))
            (package-menu-mark-internal "D" (point-min))
            (package-menu-get-command (package-menu-parse-line nil (point-min)))))
  (expect 'package-install
          (with-temp-buffer
            (package-print-package (make-package :name 'irrelevant :version '(1 2 3)))
            (package-menu-mark-internal "I" (point-min))
            (package-menu-get-command (package-menu-parse-line nil (point-min)))))
  (expect nil
          (with-temp-buffer
            (package-print-package (make-package :name 'irrelevant :version '(1 2 3)))
            (package-menu-get-command (package-menu-parse-line nil (point-min)))))
  (expect 'package-install
          (let ((package-menu-columns (list package-menu-column-name
                                            package-menu-column-version
                                            package-menu-column-status
                                            package-menu-column-command)))
            (with-temp-buffer
              (package-print-package (make-package :name 'irrelevant :version '(1 2 3)) t)
            (package-menu-mark-internal "I" (point-min))
            (package-menu-get-command (package-menu-parse-line nil (point-min))))))

  (desc "package-menu-execute")
  (expect (package 'completed)
          (with-temp-buffer
            (condition-case err
                (mocklet (((package-install *)))
                         (loop for pkg in (list test-pkg1 dep-pkg)
                               do (progn (package-print-package pkg t)
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
  (expect (error error "Expected only a single matching package, 3 found")
          (let ((package-registry `((dep-pkg
                                     ,(make-package
                                       :name 'dep-pkg
                                       :version '(0 9)
                                       :provided '(test-pkg)
                                       :archive 'elpa
                                       :type 'single
                                       :status 'obsolete)
                                     ,(make-package
                                       :name 'dep-pkg
                                       :version '(0 5)
                                       :provided '(test-pkg)
                                       :archive 'elpa
                                       :type 'single
                                       :status 'obsolete)
                                     ,(make-package
                                       :name 'dep-pkg
                                       :version '(1 9)
                                       :provided '(test-pkg)
                                       :archive 'elpa
                                       :type 'single
                                       :status 'obsolete)))))
           (package-find-rest (make-package :name 'dep-pkg))))
  (expect (package nil)
          (package-find-rest (make-package :name 'test-pkg) t))
  (expect (package tarty)
          (package-find-rest (make-package :name 'tarty) t))

  (desc "package-menu-view-commentary")
  (expect (package "Package information for test-pkg\n\nThis is a completely great testing package")
          (let (wind)
            (with-temp-buffer
              (package-print-package test-pkg1)

              (setq wind (package-menu-view-commentary)))
            (with-current-buffer (window-buffer wind)
              (buffer-string))))
  (expect (package t)
          (let (wind)
            (with-temp-buffer
              (package-print-package test-pkg1)

              (setq wind (package-menu-view-commentary)))
            (with-current-buffer (window-buffer wind)
              buffer-read-only)))
  (expect (package nil)
          (let (wind)
            (with-temp-buffer
              (package-print-package test-pkg1)

              (setq wind (package-menu-view-commentary)))
            (with-current-buffer (window-buffer wind)
              (buffer-modified-p))))

  (desc "package-list-packages")
  (expect (package "*Packages*")
          (let ((res (package-list-packages)))
            (prog1
                (buffer-name res)
              ;; Hack to make up for lack of cleanup capability.
              (kill-buffer res))))
  (expect (regexp "  dep-pkg             2.0         avail   Simple package system for Emacs *
  internal-pkg        2.0beta2    active  Simple package system for Emacs *
  tarty               1.5alpha3   avail   Simple package system for Emacs *
  test-pkg            1.0         obs     Simple package system for Emacs *
  test-pkg            1.1         active  Simple package system for Emacs *\n")
          (with-package-test
           (let ((res (package-list-packages)))
             (prog1
              (with-current-buffer res
                (buffer-string))
              (kill-buffer res)))))
  (expect (package 'package-menu-mode)
          (let ((res (package-list-packages)))
            (prog1
             (with-current-buffer res
               major-mode)
             (kill-buffer res))))

  (desc "package-from-version-1")
  (expect (make-package :name 'swank-clojure
                        :version '(1 1 0)
                        :required '((((slime-repl . (20091016)))
                                     ((clojure-mode . (1 6)))))
                        :summary "slime adapter for clojure"
                        :type 'single)
          (package-from-version-1
           '(swank-clojure .
                           [(1 1 0)
                            ((slime-repl
                              (20091016))
                             (clojure-mode
                              (1 6)))
                            "slime adapter for clojure" single])))
  (expect (make-package :name 'htmlize
                        :version '(1 37)
                        :required '(nil)
                        :summary "Convert buffer text and decorations to HTML."
                        :type 'single)
          (package-from-version-1
           '(htmlize .
                     [(1 37)
                      nil "Convert buffer text and decorations to HTML." single])))
  (expect (make-package :name 'htmlize
                        :version '(1 37)
                        :required '(nil)
                        :summary "Convert buffer text and decorations to HTML."
                        :type 'single
                        :archive 'builtin)
          (package-from-version-1
           '(htmlize .
                     [(1 37)
                      nil "Convert buffer text and decorations to HTML." single])
           'builtin))

  (desc "package-read-archive-contents")
  (expect '(1 (smex .
                    [(1 1)
                     nil "M-x interface with Ido-style lazy matching." single])
              (drag-stuff .
                          [(0 0 2)
                           nil "Drag stuff (lines, words, region, etc...) around" single]))
          (package-read-archive-contents "(1 (smex . [(1 1) nil
       \"M-x interface with Ido-style lazy matching.\" single])
 (drag-stuff .
	     [(0 0 2)
	      nil \"Drag stuff (lines, words, region, etc...) around\" single]))"))
  (expect (package '(2 (:name test-pkg
                              :version (1 0)
                              :version-raw "1.0"
                              :summary "Simple package system for Emacs"
                              :created "10 Mar 2007"
                              :updated "10 Mar 2007"
                              :license "gpl3"
                              :authors (("Joe Bob" . "jbob@example.com"))
                              :maintainer ("Joe Bob" . "jbob@example.com")
                              :provided (test-pkg)
                              :required (((dep-pkg deppy)) ())
                              :keywords ("tools" "libraries")
                              :homepage "www.example.com"
                              :wikipage "test-pkg.el"
                              :commentary "This is a completely great testing package"
                              :archive elpa
                              :type single)))
    (package-read-archive-contents upstream-archive-contents))
  (expect (error error "Package archive version 3 is not one of (2 1)")
          (package-read-archive-contents "(3 (package . [stuff]))"))

  (desc "package-register")
  (expect (package `((adder ,(make-package :name 'adder
                                           :version '(1 0)
                                           :type 'single
                                           :status 'available
                                           :archive 'manual))
                     (test-pkg ,test-pkg1 ,test-pkg2)
                     (dep-pkg ,dep-pkg)
                     (tarty ,tarty)
                     (internal-pkg ,internal-pkg)))
          (package-register (make-package :name 'adder
                                          :version '(1 0)
                                          :type 'single
                                          :archive 'manual))
          package-registry)
  (expect `((adder ,(make-package :name 'adder
                                  :version '(1 0)
                                  :type 'single
                                  :status 'available
                                  :archive 'manual)))
    (let (package-registry)
      (package-register (make-package :name 'adder
                                      :version '(1 0)
                                      :type 'single
                                      :archive 'manual))
      package-registry))
  (expect `((adder ,(make-package :name 'adder
                                  :version '(1 0)
                                  :type 'single
                                  :status 'available
                                  :archive 'manual)
                   ,(make-package :name 'adder
                                  :version '(1 1)
                                  :type 'single
                                  :status 'available
                                  :archive 'manual)))
    (let (package-registry)
      (package-register (make-package :name 'adder
                                      :version '(1 0)
                                      :type 'single
                                      :archive 'manual))
      (package-register (make-package :name 'adder
                                      :version '(1 1)
                                      :type 'single
                                      :archive 'manual))
      package-registry))
  (expect (package `((test-pkg ,test-pkg1 ,test-pkg2 ,(make-package :name 'test-pkg
                                                                    :version '(2 5)
                                                                    :type 'single
                                                                    :status 'available
                                                                    :archive 'manual))
                     (dep-pkg ,dep-pkg)
                     (tarty ,tarty)
                     (internal-pkg ,internal-pkg)))
    (package-register (make-package :name 'test-pkg
                                    :version '(2 5)
                                    :type 'single
                                    :archive 'manual))
    package-registry)
  (expect (package `((test-pkg ,test-pkg1 ,test-pkg2)
                     (dep-pkg ,dep-pkg)
                     (tarty ,tarty)
                     (internal-pkg ,internal-pkg)))
          (package-register test-pkg1)
          package-registry)
  (expect (package `((addy ,(make-package :name 'addy
                                          :version '(1 0)
                                          :type 'single
                                          :status 'available
                                          :archive 'manual))
                     (test-pkg ,test-pkg1 ,test-pkg2)
                     (dep-pkg ,dep-pkg)
                     (tarty ,tarty)
                     (internal-pkg ,internal-pkg)))
          (package-register (make-package :name 'addy
                                          :version '(1 0)
                                          :type 'single
                                          :archive 'manual))
          package-registry)
  (expect `((addy ,(make-package :name 'addy
                                 :version '(1 0)
                                 :type 'single
                                 :status 'available
                                 :archive 'manual)))
    (let (package-registry
          (addy (make-package :name 'addy
                              :version '(1 0)
                              :type 'single
                              :archive 'manual)))
      (package-register addy)
      (package-register addy)
      package-registry))
  (expect `((addy ,(make-package :name 'addy
                                 :version '(1 0)
                                 :type 'single
                                 :status 'installed
                                 :archive 'manual)))
    (let (package-registry)
      (package-register (make-package :name 'addy
                                      :version '(1 0)
                                      :type 'single
                                      :archive 'manual))
      (package-register (make-package :name 'addy
                                      :version '(1 0)
                                      :type 'single
                                      :status 'installed
                                      :archive 'manual))
      package-registry))

  (desc "package-archive-url")
  (expect (package (concat "file://" test-dir "upstream/"))
          (package-archive-url 'manual))
  (expect "http://tromey.com/elpa/"
          (package-archive-url 'elpa))
  (expect (error error "Builtin archive does not have a download URL")
          (package-archive-url 'builtin))
  (expect nil
          (package-archive-url 'builtin t))

  (desc "package-archive-content-file")
  (expect (package (concat test-dir "archive-contents"))
          (package-archive-content-file 'manual))

  (desc "package-register-archive")
  (expect (error error "Content file for archive `manual' is not readable")
    (with-package-test
     (package-register-archive 'manual)))
  (expect (package `((tarty ,tarty)))
    (setup-test 'test-dir)
    (setq package-registry nil)
    (with-temp-file (concat test-dir "upstream/" "archive-contents")
      (insert "(2" (cl-merge-pp tarty 'package) ")"))
    (package-download-one-archive 'manual)
    (package-register-archive 'manual)
    package-registry)
  (expect (error error "Package test-pkg lists elpa as its archive, but was read from archive manual")
    (with-package-test
     (setup-test 'test-dir)
     (setq package-registry nil)
     (with-temp-file (concat test-dir "upstream/" "archive-contents")
       (insert "(2" (cl-merge-pp test-pkg2 'package) ")"))
     (package-download-one-archive 'manual)
     (package-register-archive 'manual)))

  (desc "package-download-url")
  (expect "http://example.com/elpa/test-pkg-1.0.el"
          (let ((package-archives '((example "http://example.com/elpa/" "/tmp/"))))
            (package-download-url (make-package :name 'test-pkg
                                                :version '(1 0)
                                                :archive 'example
                                                :type 'single))))
  (expect "http://example.com/elpa/tarty-1.0.tar"
          (let ((package-archives '((example "http://example.com/elpa/" "/tmp/"))))
            (package-download-url (make-package :name 'tarty
                                                :version '(1 0)
                                                :archive 'example
                                                :type 'tar))))

  (desc "package-handle-response")
  (expect (regexp "^<HTML>")
    (let (str)
      (with-current-buffer (url-retrieve-synchronously "http://example.com/")
        (package-handle-response)
        (setq str (buffer-string))
        (kill-buffer)
        str)))
  (expect nil
    (let* ((buf (url-retrieve-synchronously "http://example.com/"))
           (ret (package-handle-response buf)))
      (kill-buffer buf)
      ret))
  (expect (error error "Error during download request: Not Found")
    (with-current-buffer (url-retrieve-synchronously "http://example.com/404")
      (condition-case err
          (package-handle-response)
        (error (kill-buffer)
               (signal 'error (cdr err))))))
  (expect (package "package-handle-response with a file")
    (setup-test 'test-dir)
    (with-temp-file (concat test-dir "test-file")
      (insert "package-handle-response with a file"))
    (with-current-buffer (url-retrieve-synchronously (concat "file://" test-dir "test-file"))
      (package-handle-response)
      (prog1
          (buffer-string)
        (kill-buffer))))

  (desc "package-download-one-archive")
  ;; Can't test HTTP downloads without a reliable webserver to test; that's
  ;; beyond the scope of this suite.
  (expect (package (concat test-dir "archive-contents"))
    (setup-test 'test-dir)
    (with-temp-file (concat test-dir "upstream/" "archive-contents")
      (insert upstream-archive-contents))
    (package-download-one-archive 'manual))
  (expect (package '(2 (:name test-pkg
                              :version (1 0)
                              :version-raw "1.0"
                              :summary "Simple package system for Emacs"
                              :created "10 Mar 2007"
                              :updated "10 Mar 2007"
                              :license "gpl3"
                              :authors (("Joe Bob" . "jbob@example.com"))
                              :maintainer ("Joe Bob" . "jbob@example.com")
                              :provided (test-pkg)
                              :required (((dep-pkg deppy)) nil)
                              :keywords ("tools" "libraries")
                              :homepage "www.example.com"
                              :wikipage "test-pkg.el"
                              :commentary "This is a completely great testing package"
                              :archive elpa
                              :type single)))
    (setup-test 'test-dir)
    (with-temp-file (concat test-dir "upstream/" "archive-contents")
      (insert upstream-archive-contents))
    (package-download-one-archive 'manual)
    (let ((buf (find-file-noselect (package-archive-content-file 'manual))))
      (prog1
          (package-read-archive-contents buf)
        (kill-buffer buf))))
  (expect (package (concat test-dir "manual/"))
    (let ((package-archives `((manual ,(concat "file://" test-dir "upstream/") ,(concat test-dir "manual/")))))
      (make-directory (concat test-dir "upstream/") t)
     (with-temp-file (concat test-dir "upstream/" "archive-contents")
       (insert upstream-archive-contents))
     (package-download-one-archive 'manual)
     (package-archive-localpath 'manual)))

  (desc "package-download")
  (expect (package `((test-pkg ,test-pkg1 ,test-pkg2)
                     (dep-pkg ,dep-pkg)
                     (tarty ,(cl-merge-struct 'package (copy-package tarty) (make-package :status 'installed)))
                     (internal-pkg ,internal-pkg)))
    (setup-test 'test-dir 'tarty)
    (with-temp-file (concat test-dir "upstream/" "archive-contents")
      (insert upstream-archive-contents))
    (package-download tarty)
    package-registry)

  (desc "package-read-sexp")
  (expect '(a good parsed sexp)
    (package-read-sexp "(a good parsed sexp)"))
  (expect (error error "Can't read whole string")
    (package-read-sexp "(multiple) (sexps)"))

  (desc "package-from-string")
  (expect (make-package :name 'test-pkg
                        :version '(1 0)
                        :version-raw "1.0"
                        :summary "Simple package system for Emacs"
                        :created "10 Mar 2007"
                        :updated "10 Mar 2007"
                        :license "gpl3"
                        :authors '(("Joe Bob" . "jbob@example.com"))
                        :maintainer '("Joe Bob" . "jbob@example.com")
                        :provided '(test-pkg)
                        :required '(((dep-pkg deppy)))
                        :keywords '("tools" "libraries")
                        :homepage "www.example.com"
                        :wikipage "test-pkg.el"
                        :commentary "This is a completely great testing package"
                        :archive 'elpa
                        :type 'single
                        :status 'obsolete)
    (package-from-string "(:name test-pkg
                           :version (1 0)
                           :version-raw \"1.0\"
                           :summary \"Simple package system for Emacs\"
                           :created \"10 Mar 2007\"
                           :updated \"10 Mar 2007\"
                           :license \"gpl3\"
                           :authors ((\"Joe Bob\" . \"jbob@example.com\"))
                           :maintainer (\"Joe Bob\" . \"jbob@example.com\")
                           :provided (test-pkg)
                           :required (((dep-pkg deppy)))
                           :keywords (\"tools\" \"libraries\")
                           :homepage \"www.example.com\"
                           :wikipage \"test-pkg.el\"
                           :commentary \"This is a completely great testing package\"
                           :archive elpa
                           :type single
                           :status obsolete)"))
  (expect (error error "Keyword argument bad-spec not one of (:name :version :version-raw :summary :created :updated :license :authors :maintainer :adapted-by :provided :required :keywords :homepage :wikipage :commentary :archive :type :status)")
    (package-from-string "(bad-spec)"))
  (expect (error error "Can't read whole string")
    (package-from-string "(sexp1) (sexp2)"))
  (expect nil
    (package-from-string "(sexp1) (sexp2)" t))

  (desc "package-load-rest-from-descriptor")
  (expect (package (make-package :name 'tarty
                                 :version '(1 5 -3 3)
                                 :version-raw "1.5alpha3"
                                 :summary "Simple package system for Emacs"
                                 :created "10 Mar 2007"
                                 :updated "10 Mar 2007"
                                 :license "gpl3"
                                 :authors '(("George Tarterson" . "jtart@example.com"))
                                 :maintainer '("George Tarterson" . "jtart@example.com")
                                 :provided '(tarty)
                                 :keywords '("tools" "libraries")
                                 :homepage "tarty.example.com"
                                 :wikipage "tarty.el"
                                 :commentary "This is a completely great testing package"
                                 :archive 'manual
                                 :type 'tar
                                 :status 'available))
    (setup-test 'test-dir)
    (make-directory (concat test-dir "tarty-1.5alpha3"))
    (with-temp-file (concat test-dir "tarty-1.5alpha3/info.epkg")
      (insert (cl-merge-pp tarty 'package)))
    (package-load-rest-from-descriptor (make-package :name 'tarty
                                                     :version '(1 5 -3 3)
                                                     :archive 'manual)))
  (expect (package (make-package :name 'test-pkg
                                 :version '(1 0)
                                 :version-raw "1.0"
                                 :archive 'elpa
                                 :type 'single
                                 :status 'obsolete))
    (setup-test 'test-dir)
    (make-directory (concat test-dir "test-pkg-1.0"))
    (let ((package-archives `((elpa ,(concat "file://" test-dir "upstream/") ,test-dir))))
      (with-temp-file (concat test-dir "test-pkg-1.0/info.epkg")
        (insert "(:name test-pkg
                :version (1 0)
                :version-raw \"1.0\"
                :archive elpa
                :type single
                :status obsolete)"))
      (package-load-rest-from-descriptor (make-package :name 'test-pkg
                                                       :version '(1 0)
                                                       :archive 'elpa))))
  (expect (package (make-package :name 'test-pkg
                                 :version '(1 0)
                                 :version-raw "1.0"
                                 :archive 'elpa
                                 :type 'single
                                 :status 'obsolete))
    (setup-test 'test-dir)
    (make-directory (concat test-dir "test-pkg-1.0"))
    (let ((package-archives `((elpa ,(concat "file://" test-dir "upstream/") ,test-dir)))
          (pkg (make-package :name 'test-pkg
                             :version '(1 0)
                             :archive 'elpa)))
      (with-temp-file (concat test-dir "test-pkg-1.0/info.epkg")
        (insert "(:name test-pkg
                :version (1 0)
                :version-raw \"1.0\"
                :archive elpa
                :type single
                :status obsolete)"))
      (package-load-rest-from-descriptor pkg)
      pkg))
  (expect (error error "Unable to load package info file '/a/non/existent/path/bad-1.0/info.epkg'")
    (let ((package-archives '((fake "file:///a/non/existent/path/" "/a/non/existent/path/"))))
      (package-load-rest-from-descriptor (make-package :name 'bad
                                                       :version '(1 0)
                                                       :archive 'fake))))
  (expect nil
    (let ((package-archives '((fake "file:///a/non/existent/path/" "/a/non/existent/path/"))))
      (package-load-rest-from-descriptor (make-package :name 'bad
                                                       :version '(1 0)
                                                       :archive 'fake)
                                         t)))

  (desc "package-register-all-installed")
  (expect (package nil)
    (let (package-registry)
     (setup-test 'test-dir)
     (package-register-all-installed)
     package-registry))
  (expect (package `((tarty ,(cl-merge-struct 'package
                                              (copy-package tarty)
                                              (make-package
                                               :status 'installed)))))
          (let (package-registry)
            (setup-test 'test-dir 'tarty)
            (package-download tarty)
            (package-register-all-installed)
            package-registry))
  (expect (package `((tarty ,(cl-merge-struct 'package
                                              (copy-package tarty)
                                              (make-package
                                               :status 'installed)))))
    (let ((package-registry `((tarty ,tarty))))
      (setup-test 'test-dir 'tarty)
      (package-download tarty)
      (package-register-all-installed)
      package-registry))
  (expect (package `((simple-file ,(cl-merge-struct 'package
                                                    (copy-package simple-file-pkg)
                                                    (make-package :status 'installed)))
                     (tarty ,tarty)))
    (let ((package-registry `((tarty ,tarty))))
      (setup-test 'test-dir 'tarty)
      (make-directory (concat test-dir "simple-file-1.2.3"))
      (with-temp-file (concat test-dir "simple-file-1.2.3/info.epkg")
        (insert (cl-merge-pp simple-file-pkg 'package)))
      (package-register-all-installed)
      package-registry))

  (desc "package-install-file-path")
  (expect (error error "Package type must be `single' to get an install file; given: builtin")
          (package-install-file-path (make-package :name 'more-good
                                                   :version '(1 2 3)
                                                   :type 'builtin)))
  (expect (error error "Package type must be `single' to get an install file; given: tar")
          (package-install-file-path (make-package :name 'more-good
                                                   :version '(1 2 3)
                                                   :type 'tar)))
  (expect (package (concat test-dir "single-file-1.2.34/single-file.el"))
          (package-install-file-path (make-package :name 'single-file
                                                   :version '(1 2 34)
                                                   :archive 'manual
                                                   :type 'single)))
  (desc "package-do-activate")
  (expect (package 'activated)
    (setup-test 'test-dir 'tarty)
    (package-download tarty)
    (package-do-activate tarty))
  (expect (package t)
    (setup-test 'test-dir 'tarty)
    (package-download tarty)
    (package-do-activate tarty)
    (consp (member (concat test-dir "tarty-1.5alpha3/") load-path)))
  (expect (package (concat test-dir "tarty-1.5alpha3/tarty-autoloads.el"))
    (setup-test 'test-dir 'tarty)
    (package-download tarty)
    (package-do-activate tarty)
    (caar load-history))
  (expect (package `(tarty ,(cl-merge-struct 'package
                                             (copy-package tarty)
                                             (make-package :status 'activated))))
    (setup-test 'test-dir 'tarty)
    (package-download tarty)
    (package-do-activate tarty)
    (assq 'tarty package-registry))

  (desc "package-activate")
  (expect (package nil)
    (package-activate test-pkg2))
  (expect (package nil)
    (package-activate (make-package :name 'emacs
                                    :version '(23 1 0))))
  (expect (error file-error "Cannot open load file")
    (with-package-test
     (let ((package-archives `((elpa ,(concat "file://" test-dir "upstream/") ,test-dir))))
      (package-activate test-pkg1))))
  (expect (mock (package-find-latest 'dep-pkg nil *) => (make-package :name 'dep-pkg :version '(2 0)))
    (mocklet ((package-do-activate))
     (with-package-test
      (let ((package-archives `((elpa ,(concat "file://" test-dir "upstream/") ,test-dir))))
        (package-activate test-pkg1)))))
  (expect (not-called package-do-activate)
    (package-activate nil))

  (desc "package-generate-autoloads")
  (expect (regexp ";;; tarty-autoloads\.el --- automatically extracted autoloads")
    (with-package-test
     (setup-test 'test-dir 'tarty)
     (package-download tarty)
     (package-generate-autoloads tarty)
     (with-temp-buffer
       (insert-file-contents (package-autoload-file tarty))
       (buffer-string))))
  (expect (regexp "(provide 'tarty-autoloads)")
    (with-package-test
     (setup-test 'test-dir 'tarty)
     (package-download tarty)
     (package-generate-autoloads tarty)
     (with-temp-buffer
       (insert-file-contents (package-autoload-file tarty))
       (buffer-string))))

  )

(provide 'package-test)

;;; package-test.el ends here
