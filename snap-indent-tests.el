;;; snap-indent-tests.el --- Snap-indent tests

;; Copyright (C) 2022 Jeff Valk

;; Author: Jeff Valk <jv@jeffvalk.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for snap-indent

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'snap-indent)

(ert-deftest snap-indent-as-list-test ()
  "Test function list wrapping, particularly with lambdas."
  (let* ((lam1 (lambda () 'dummy-value))
         (lam2 (function (lambda () 'dummy-value)))
         (sym1 'tabify)
         (syms1 '(tabify untabify delete-trailing-whitespace))
         (list1 (cons lam1 syms1))
         (list2 (append syms1 (list lam1 lam2 (lambda () 'dummy-value)))))
    (should (equal (snap-indent-as-list lam1) (list lam1)))
    (should (equal (snap-indent-as-list lam2) (list lam2)))
    (should (equal (snap-indent-as-list sym1) (list sym1)))
    (should (equal (snap-indent-as-list syms1) syms1))
    (should (equal (snap-indent-as-list list1) list1))
    (should (equal (snap-indent-as-list list2) list2))
    (should (cl-every #'functionp (snap-indent-as-list list1)))
    (should (cl-every #'functionp (snap-indent-as-list list2)))))

(provide 'snap-indent-tests)

;;; snap-indent-tests.el ends here
