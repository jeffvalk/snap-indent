# Snap-indent

[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)

## Synopsis

Snap-indent provides simple automatic indentation for Emacs as a minor mode. It enables the following features:

- On yank/paste, indent inserted text according to major mode
- On save, indent buffer text according to major mode (optional)
- When indenting, further format text, e.g. tabify, untabify (optional)
- Prevent minor mode activation in certain major modes (optional)

Snap-indent's additional formatting behavior is very flexible. Any function that operates on a region may be used, and multiple functions may be specified.

### Complement to electric-indent-mode

`electric-indent-mode` indents while typing. Snap-indent indents on yank/paste and optionally on save. These do not overlap, and work well together.

### Replacement for auto-indent-mode

Snap-indent was created as a minimalist replacement for `auto-indent-mode`. That package is shockingly complex, and while I respect the ambition of its author, complexity has costs. At the time snap-indent was created, `auto-indent-mode` had numerous interoperability bugs with other common packages. Its approach hadn't aged well.

In contrast, snap-indent, while quite flexible, is designed for simplicity and hygiene. It doesn't replace built-in functionality or use function advice. It's lightweight and should play well with other packages. And it has regression tests.

## Installation

Currently, snap-indent is not uploaded to any package repository. Loading the package from source is required.

## Usage

Execute `M-x snap-indent-mode` to enable in any buffer, or add a hook to enable for a specific mode:

```elisp
(add-hook prog-mode-hook #'snap-indent-mode)
```

To configure via `use-package`, adapt the following example as desired:

```elisp
(use-package snap-indent
  :load-path "site-lisp/"
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format 'untabify)
           (snap-indent-on-save t)))
```

### Options

| Option                       | Type             | Default          | Description                                   |
|:-----------------------------|:-----------------|:-----------------|:----------------------------------------------|
| `snap-indent-excluded-modes` | symbol list      | `'(python-mode)` | Major modes in which to ignore activation     |
| `snap-indent-format`         | function or list | `nil`            | Additional formatting to apply when indenting |
| `snap-indent-on-save`        | boolean          | `nil`            | Whether to indent the entire buffer on save   |

### Custom formatting

Snap-indent can optionally apply additional formatting when indenting. This is highly customizable and controlled by the variable `snap-indent-format`. When non-nil, this variable may be either a single function or a list of functions to apply sequentially:

```elisp
(setq snap-indent-format 'untabify)                                  ; single function
(setq snap-indent-format '(untabify delete-trailing-whitespace ...)) ; list of functions
```

Each function must accept two arguments, which specify the start and end positions of the region on which to operate. Functions may be specified as symbols or lambda forms.

## License

Copyright © 2022 Jeff Valk

Distributed under the GNU General Public License, version 3
