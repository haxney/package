;;; multi-file-sub.el --- A dependent file within a package.

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Jane Smith <jsmith@example.com>
;; Keywords: lisp, tools
;; Package: multi-file
;; Version: 0.2.3

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The main multi-file elisp source depends on this.

;;; Code:

(defun multi-file-frobnicate (count)
  "Frobnicate the current buffer COUNT times."
  (interactive "p")
  (dotimes (frobs count)
   (insert "frobnicated " frobs "!\n")))

(provide 'multi-file-sub)

;;; multi-file-sub.el ends here
