;;; multi-file.el --- Example of a multi-file tar package

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

;; This is the main file of the "multi-file" package.

;;; Code:

(require 'multi-file-sub)

(defgroup multi-file nil
  "Multi-file example"
  :group 'development)

;;;###autoload
(defcustom multi-file-custom-var nil
  "An example autoloaded custom variable"
  :type 'boolean
  :group 'multi-file)

(defvar multi-file-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'multi-file-frobnicate)
    map)
  "Keymap for `multi-file-mode'")



;;;###autoload
(define-derived-mode multi-file-mode text-mode "Multi"
  "Major mode which does nothing but test things.

The keys are:
\\{multi-file-mode-map}"
  :group 'multi-file
  (turn-off-auto-fill)
  (set (make-local-variable 'comment-start) ";")
  (setq case-fold-search nil))

(add-to-list 'auto-mode-alist '("\\.multi\\'" . multi-file-mode))

(provide 'multi-file)

;;; multi-file.el ends here
