;;; snap-indent.el --- Simple automatic indentation -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jeff Valk

;; Author: Jeff Valk <jv@jeffvalk.com>
;; URL: https://github.com/jeffvalk/snap-indent
;; Keywords: indent tools convenience
;; Version: 1.1
;; Package-Requires: ((emacs "24.1"))

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

;; Snap-indent provides simple automatic indentation (and optional formatting)
;; when yanking/pasting text. It was inspired by `auto-indent-mode', and is
;; designed for improved simplicity, flexibility, and interoperability.
;;
;; `snap-indent-mode' is an Emacs minor mode that enables the following
;; features:
;;
;; - Indent inserted text according to major mode on yank/paste
;; - Indent buffer text according to major mode on save (optional)
;; - When indenting, additionally format text, e.g. tabify, untabify, remove
;;   trailing whitespace, etc (optional)
;; - Prevent minor mode activation in certain major modes (optional)
;; - Skip indentation systematically by maximum text length or according to any
;;   user-defined predicate (optional)
;; - Skip indentation for a single operation using an argument prefix (optional)
;;
;; Snap-indent's additional formatting behavior is very flexible. Any function
;; that operates on a region may be used, and multiple functions may be
;; specified.
;;
;; Snap-indent can be configured to skip indentation with equal flexibility.
;; Any predicate function can be set control this behavior systematically, and
;; indentation may be suppressed for a single operation with key input.

;;; Code:

(defgroup snap-indent nil
  "Customization group for snap-indent."
  :prefix "snap-indent-"
  :group 'indent)

(defcustom snap-indent-excluded-modes '(cmake-ts-mode
                                        coffee-mode
                                        conf-mode
                                        elm-mode
                                        haml-mode
                                        haskell-mode
                                        makefile-automake-mode
                                        makefile-bsdmake-mode
                                        makefile-gmake-mode
                                        makefile-imake-mode
                                        makefile-makepp-mode
                                        makefile-mode
                                        occam-mode
                                        python-mode
                                        python-ts-mode
                                        slim-mode
                                        yaml-mode
                                        yaml-ts-mode)
  "Modes in which `snap-indent-mode' should not be activated.
This permits activation for `prog-mode' while excluding certain modes derived
from `prog-mode', for example."
  :type '(repeat symbol)
  :group 'snap-indent)

(defcustom snap-indent-on-save nil
  "Whether to indent the entire buffer on save."
  :type 'boolean
  :group 'snap-indent)

(defcustom snap-indent-format nil
  "Additional formatting function(s) to apply when indenting.
This may be a single function, a list of functions, or nil. The function(s) must
accept two arguments, which specify the start and end positions of the region on
which to operate. Useful built-in functions include `tabify' and `untabify' for
tab/space conversion and `delete-trailing-whitespace'."
  :type '(choice
          (function :tag "Single function")
          (repeat :tag "List of functions" function))
  :group 'snap-indent)

(defcustom snap-indent-length-limit nil
  "Maximum text length to indent.
Set this to prevent any performance issues with large blocks of text.
When nil, no limit is applied."
  :type 'integer
  :group 'snap-indent)

(defcustom snap-indent-skip-on-prefix-arg nil
  "Whether a prefix command argument causes indentation to be skipped.
When non-nil, this lets you skip indentation for a single operation without
disabling `snap-indent-mode'."
  :type 'boolean
  :group 'snap-indent)

(defcustom snap-indent-skip-on-condition nil
  "Predicate function to cause indentation to be skipped.
When specified, this lets you skip indentation for a single operation without
disabling `snap-indent-mode' according to any logic you choose.

The function must accept two arguments, which specify the start and end
positions of the region on which to (potentially) operate. The function should
return non-nil to skip indentation, and nil otherwise."
  :type 'function
  :group 'snap-indent)

;; To make user configuration more expressive and less error-prone,
;; `snap-indent-format' may be either a function or a list of functions; if the
;; former, we'll wrap it in a list. Caveat when checking for this: lambdas are
;; both functions and lists. (The lambda form is self-quoting; evaluating it
;; returns the form itself.) Hence, to distinguish what should be wrapped, we
;; must test the value's function-ness not just its list-ness.

(defun snap-indent-as-list (function-or-list)
  "Return FUNCTION-OR-LIST as a list, treating lambda forms as atoms."
  (if (or (not (listp function-or-list)) (functionp function-or-list))
      (list function-or-list)
    function-or-list))

(defun snap-indent-indent (beg end)
  "Indent and optionally format the text between BEG and END."
  (let ((transient-mark-mode nil)
        (orig-max (point-max)))
    (indent-region beg end)
    (dolist (format (snap-indent-as-list snap-indent-format))
      (let ((end* (+ end (- (point-max) orig-max)))) ; account for prior changes
        (funcall format beg end*)))))

(defun snap-indent-maybe-indent (beg end)
  "If the region between BEG and END should be indented, dispatch that action."
  (unless (or (and snap-indent-length-limit
                   (> (- end beg) snap-indent-length-limit))
              (and snap-indent-skip-on-prefix-arg
                   current-prefix-arg)
              (and snap-indent-skip-on-condition
                   (funcall snap-indent-skip-on-condition beg end)))
    (snap-indent-indent beg end)))

(defun snap-indent-save-handler ()
  "Indent buffer text on save as specified."
  (when snap-indent-on-save
    (snap-indent-maybe-indent (point-min) (point-max))))

(defun snap-indent-command-handler ()
  "Indent region text on yank."
  (when (memq this-command '(yank yank-pop))
    (snap-indent-maybe-indent (region-beginning) (region-end))))

;;;###autoload
(define-minor-mode snap-indent-mode
  "Toggle snap-indent mode on or off.
Turn snap-indent on if ARG is positive, or off otherwise."
  :init-value nil
  :global nil
  :lighter " Snap"
  (if (and snap-indent-mode
           (not (memq major-mode snap-indent-excluded-modes)))
      (progn
        (add-hook 'before-save-hook #'snap-indent-save-handler nil 'local)
        (add-hook 'post-command-hook #'snap-indent-command-handler nil 'local))
    (progn
      (remove-hook 'before-save-hook #'snap-indent-save-handler 'local)
      (remove-hook 'post-command-hook #'snap-indent-command-handler 'local))))

(provide 'snap-indent)

;;; snap-indent.el ends here
