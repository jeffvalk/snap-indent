# Snap-indent

[![Build Status](https://github.com/jeffvalk/snap-indent/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/jeffvalk/snap-indent/actions)
[![MELPA](https://melpa.org/packages/snap-indent-badge.svg)](https://melpa.org/#/snap-indent)
[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

## Synopsis

Snap-indent provides simple automatic indentation (and optional formatting) when yanking/pasting text. It was inspired by [auto-indent-mode](https://github.com/mattfidler/auto-indent-mode.el), and is designed for improved simplicity, flexibility, and interoperability.

`snap-indent-mode` is an Emacs minor mode that enables the following features:

- Indent inserted text according to major mode on yank/paste
- Indent buffer text according to major mode on save (optional)
- When indenting, additionally format text, e.g. tabify, untabify, remove trailing whitespace, etc (optional)
- Prevent minor mode activation in certain major modes (optional)
- Skip indentation systematically by maximum text length or according to any user-defined predicate (optional)
- Skip indentation for a single operation using an argument prefix (optional)

Snap-indent's additional formatting behavior is very flexible. Any function that operates on a region may be used, and multiple functions may be specified.

Snap-indent can be configured to skip indentation with equal flexibility. Any predicate function can be set control this behavior systematically, and indentation may be suppressed for a single operation with key input.

## Related packages

### Complement to electric-indent-mode

`electric-indent-mode` indents while typing. Snap-indent indents on yank/paste and optionally on save. These do not overlap, and work well together.

### Replacement for auto-indent-mode

Snap-indent was created as a minimalist replacement for `auto-indent-mode`. That package is surprisingly complex, and while I respect the ambition of its author, complexity has costs. At the time snap-indent was created, `auto-indent-mode` had numerous interoperability bugs with other common packages. Its approach hadn't aged well.

In contrast, snap-indent, while quite flexible, is designed for simplicity and hygiene. It doesn't overwrite built-in functionality or use function advice. It's lightweight and should play well with other packages. And it has unit tests.

## Installation

Snap-indent is available from [MELPA](https://melpa.org/). 

## Usage

Execute `M-x snap-indent-mode` to enable in any buffer, or add a hook to enable for a specific mode:

```elisp
(add-hook 'prog-mode-hook #'snap-indent-mode)
```

To configure via `use-package`, adapt the following example as desired:

```elisp
(use-package snap-indent
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format 'untabify)
           (snap-indent-on-save t)))
```

### Customization

The following customization variables are available:

| Variable                         | Type             | Default           | Description                                                |
|:---------------------------------|:-----------------|:------------------|:-----------------------------------------------------------|
| `snap-indent-excluded-modes`     | symbol list      | [See list][modes] | Major modes in which to ignore activation                  |
| `snap-indent-format`             | function or list | `nil`             | Additional formatting to apply when indenting              |
| `snap-indent-on-save`            | boolean          | `nil`             | Whether to indent the entire buffer on save                |
| `snap-indent-length-limit`       | integer          | `nil`             | Maximum text length to indent                              |
| `snap-indent-skip-on-prefix-arg` | boolean          | `nil`             | Whether a prefix argument causes indentation to be skipped |
| `snap-indent-skip-on-condition`  | function         | `nil`             | Predicate function to cause indentation to be skipped      |

[modes]: snap-indent.el#L60-L77

### Additional formatting

Snap-indent can optionally apply additional formatting when indenting. This is highly customizable and controlled by the variable `snap-indent-format`. When non-nil, this variable may be either a single function or a list of functions to apply sequentially:

```elisp
(setq snap-indent-format 'untabify)                                  ; single function
(setq snap-indent-format '(untabify delete-trailing-whitespace ...)) ; list of functions
```

Each function must accept two arguments: the beginning and end positions of the region on which to operate. Functions may be specified as symbols or lambda forms. Useful built-in functions include `tabify` and `untabify` for tab/space conversion and `delete-trailing-whitespace`.

### Controlling activation

- **Prevention by major mode**. To prevent enabling of `snap-indent-mode` in certain major modes, include these in the `snap-indent-excluded-modes` list. This permits activation for `prog-mode` while excluding certain modes derived from `prog-mode`, for example.

- **Skipping individual operations**.
When `snap-indent-mode` is enabled, its indentation behavior may still be skipped for an individual operation.

    - To skip indentation on large blocks for text (for performance reasons), set `snap-indent-length-limit`.
    - To skip indentation manually for a single command, set `snap-indent-skip-on-prefix-arg` to `t`, then use a prefix argument (e.g. `C-u`) when invoking the command.
    - To skip indentation systematically according to any user-specified logic, set `snap-indent-skip-on-condition` to a predicate function. The function must accept two arguments, which specify the start and end positions of the region on which to (potentially) operate. The function should return non-nil to skip indentation, and nil otherwise.

## License

Copyright © 2023 Jeff Valk

Distributed under the GNU General Public License, version 3
