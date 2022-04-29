;;; snap-indent.el --- Simple automatic indentation

;; Copyright (C) 2022 Jeff Valk

;; Author: Jeff Valk <jv@jeffvalk.com>
;; URL: https://github.com/jeffvalk/snap-indent
;; Keywords: indent tools convenience
;; Version: 1.0-snapshot

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

;; Snap-indent provides a simple automatic indentation minor mode for Emacs.
;; It enables the following features:
;;
;;  - On yank/paste, indent inserted text according to major mode
;;  - On save, indent buffer text according to major mode (optional)
;;  - When indenting, further format text, e.g. tabify, untabify (optional)

;;; Code:

(defgroup snap-indent nil
  "Customization group for snap-indent."
  :prefix "snap-indent-"
  :group 'indent)

(defcustom snap-indent-excluded-modes nil
  "Modes in which `snap-indent-mode' should not be activated.
This permits activation for `prog-mode' while excluding certain modes derived
from `prog-mode', for example."
  :type '(repeat symbol)
  :group 'snap-indent)

(defcustom snap-indent-format nil
  "Additional formatting function to apply when indenting.
Commonly, this may be `tabify' or `untabify'. The function must accept two
arguments which indicate the start and end positions for the region to format.
If no additional formatting is desired, this should be nil."
  :type 'function
  :options '(nil tabify untabify)
  :group 'snap-indent)

(defcustom snap-indent-on-save nil
  "Whether to indent the entire buffer on save."
  :type 'boolean
  :group 'snap-indent)

(defun snap-indent-indent (beg end)
  "Indent the text between BEG and END."
  (let ((transient-mark-mode nil)
        (orig-max (point-max)))
    (indent-region beg end)
    (when snap-indent-format
      (let ((end* (+ end (- (point-max) orig-max)))) ; change from indent
        (funcall snap-indent-format beg end*)))))

(defun snap-indent-save-handler ()
  "Indent buffer text on save as specified."
  (when snap-indent-on-save
    (snap-indent-indent (point-min) (point-max))))

(defun snap-indent-command-handler ()
  "Indent region text on yank."
  (when (memq this-command '(yank yank-pop))
    (snap-indent-indent (region-beginning) (region-end))))

(defun snap-indent-enabled-buffers ()
  "Return the list of buffers for which `snap-indent-mode' is enabled."
  (remove nil
          (mapcar (lambda (buf)
                    (with-current-buffer buf
                      (when snap-indent-mode
                        buf)))
                  (buffer-list))))

;;;###Autoload
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
