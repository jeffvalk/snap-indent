# Snap-indent

[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)

## Synopsis

Snap-indent provides a simple automatic indentation minor mode for Emacs. It enables the following features:

- On yank/paste, indent inserted text according to major mode
- On save, indent buffer text according to major mode (optional)
- When indenting, further format text, e.g. tabify, untabify (optional)

### Replacement for auto-indent-mode

Snap-indent was created as a minimalist replacement for `auto-indent-mode`, which is surprisingly complicated and heavy for the task at hand. While I respect the ambition of its author, its complexity and approach introduce interoperability bugs with other common packages.

In contrast, snap-indent, while quite flexible, is designed for simplicity and hygiene. It doesn't replace core functionality or use function advice. It's lightweight and should play well with other packages.

## Installation

Currently, snap-indent is not uploaded to any package repository. Loading the package from source is required.

## Usage

Execute `M-x snap-indent-mode` to enable in any buffer, or add a hook to enable for a specific mode:

```elisp
(add-hook emacs-lisp-mode-hook #'snap-indent-mode)
(add-hook c-mode-hook #'snap-indent-mode)
```

To configure via `use-package`, adapt the following example as desired:

```elisp
(use-package snap-indent
  :load-path "site-lisp/"
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format 'untabify)
           (snap-indent-on-save t)))
```

## License

Copyright © 2022 Jeff Valk

Distributed under the GNU General Public License, version 3
