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
  `(let ((package-archive-contents '((dog . (,(make-package
                                               :name 'dog
                                               :version '(1 0)
                                               :version-raw "1.0"
                                               :summary "summ"
                                               :created "123"
                                               :updated "123"
                                               :license "gpl3"
                                               :authors '()
                                               :maintainer '()
                                               :provides '(dog mouse)
                                               :requires-hard '()
                                               :requires-soft '()
                                               :keywords '("key1" "key2")
                                               :homepage "www.example.com"
                                               :wikipage "wiki"
                                               :commentary "comment")
                                             ,(make-package
                                               :name 'dog
                                               :version '(1 1)
                                               :version-raw "1.1"
                                               :summary "summ"
                                               :created "123"
                                               :updated "123"
                                               :license "gpl3"
                                               :authors '()
                                               :maintainer '()
                                               :provides '(dog mouse)
                                               :requires-hard '()
                                               :requires-soft '()
                                               :keywords '("key1" "key2")
                                               :homepage "www.example.com"
                                               :wikipage "wiki"
                                               :commentary "comment"))))))
     (unwind-protect
         (progn
           ,@body))))

(expectations
  (expect 'dog
    (with-package-test
     (caar package-archive-contents))
    )
  (expect 'cat
    (with-package-test
     (setq package-archive-contents 'q)
    (error "no")
     'cat)
    )
  )

(provide 'package-test)

;;; package-test.el ends here
