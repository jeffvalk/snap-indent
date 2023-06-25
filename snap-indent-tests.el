;;; snap-indent-tests.el --- Snap-indent tests -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jeff Valk

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

(ert-deftest snap-indent-indent-test ()
  "Test indentation and formatting."
  ;; The tests below rely on equivalence of indentation behavior between elisp
  ;; major mode formatting and pretty printing. If these ever break suddenly,
  ;; check this assumption.
  (let* ((inhibit-message t) ; run tests quietly
         (forms '(lorem ipsum dolor
                        (sit amet consectetur (adipiscing elit))
                        (sed () do eiusmod
                             (tempor incididunt) (ut (labore (et (dolore ())))))
                        (magna aliqua ut)
                        (((enim)) ad (minim) veniam)
                        (quis nostrud exercitation ullamco laboris nisi)))
         (pp-text (pp-to-string forms))
         (unindented-text (replace-regexp-in-string "^ *" "" pp-text))
         (trailing-ws-text (replace-regexp-in-string "\n" "    \n" unindented-text)))
    ;; Indentation only, no formatting
    (with-temp-buffer
      (emacs-lisp-mode)
      (snap-indent-mode)
      (let ((snap-indent-format nil))
        (insert unindented-text)
        (snap-indent-indent (point-min) (point-max))
        (should (string-equal (buffer-string) pp-text))))
    ;; Indentation with one formatting function
    (with-temp-buffer
      (emacs-lisp-mode)
      (snap-indent-mode)
      (let ((snap-indent-format 'delete-trailing-whitespace))
        (insert trailing-ws-text)
        (snap-indent-indent (point-min) (point-max))
        (should (string-equal (buffer-string) pp-text))))
    ;; Indentation with multiple formatting functions
    (with-temp-buffer
      (emacs-lisp-mode)
      (snap-indent-mode)
      (let ((snap-indent-format '(delete-trailing-whitespace upcase-region)))
        (insert trailing-ws-text)
        (snap-indent-indent (point-min) (point-max))
        (should (string-equal (buffer-string) (upcase pp-text)))))
    ;; Ensure effects are confined to region
    (with-temp-buffer
      (emacs-lisp-mode)
      (snap-indent-mode)
      (let* ((snap-indent-format nil)
             (beg (length unindented-text))
             (end (* beg 2)))
        (insert unindented-text
                unindented-text
                unindented-text)
        (snap-indent-indent beg end) ; format only second text region
        (should (string-equal (buffer-string)
                              (concat unindented-text
                                      pp-text
                                      unindented-text)))))))

(ert-deftest snap-indent-maybe-indent-test ()
  "Test conditional indentation."
  ;; The tests below rely on equivalence of indentation behavior between elisp
  ;; major mode formatting and pretty printing. If these ever break suddenly,
  ;; check this assumption.
  (let* ((inhibit-message t) ; run tests quietly
         (forms '(lorem ipsum dolor
                        (sit amet consectetur (adipiscing elit))
                        (sed () do eiusmod
                             (tempor incididunt) (ut (labore (et (dolore ())))))
                        (magna aliqua ut)
                        (((enim)) ad (minim) veniam)
                        (quis nostrud exercitation ullamco laboris nisi)))
         (pp-text (pp-to-string forms))
         (unindented-text (replace-regexp-in-string "^ *" "" pp-text)))
    ;; Skip when exceeding length limit
    (with-temp-buffer
      (emacs-lisp-mode)
      (snap-indent-mode)
      (insert unindented-text)
      (let ((snap-indent-length-limit 10))    ; over limit
        (snap-indent-maybe-indent (point-min) (point-max))
        (should (string-equal (buffer-string) unindented-text)))
      (let ((snap-indent-length-limit 10000)) ; under limit
        (snap-indent-maybe-indent (point-min) (point-max))
        (should (string-equal (buffer-string) pp-text))))
    ;; Skip when prefix arg is specified
    (with-temp-buffer
      (emacs-lisp-mode)
      (snap-indent-mode)
      (insert unindented-text)
      (let ((snap-indent-skip-on-prefix-arg t)
            (current-prefix-arg '(4))) ; prefixed
        (snap-indent-maybe-indent (point-min) (point-max))
        (should (string-equal (buffer-string) unindented-text)))
      (let ((snap-indent-skip-on-prefix-arg t)
            (current-prefix-arg nil))  ; not prefixed
        (snap-indent-maybe-indent (point-min) (point-max))
        (should (string-equal (buffer-string) pp-text))))
    ;; Skip according to user-defined predicate
    (with-temp-buffer
      (emacs-lisp-mode)
      (snap-indent-mode)
      (insert unindented-text)
      (let ((snap-indent-skip-on-condition (lambda (_ _) t)))    ; pred: t
        (snap-indent-maybe-indent (point-min) (point-max))
        (should (string-equal (buffer-string) unindented-text)))
      (let ((snap-indent-skip-on-condition (lambda (_ _) nil)))  ; pred: nil
        (snap-indent-maybe-indent (point-min) (point-max))
        (should (string-equal (buffer-string) pp-text))))))

(provide 'snap-indent-tests)

;;; snap-indent-tests.el ends here
