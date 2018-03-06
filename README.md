# flycheck-posframe

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/flycheck-posframe-badge.svg)](http://melpa.org/#/flycheck-posframe)
[![Build Status](https://travis-ci.org/alexmurray/flycheck-posframe.svg?branch=master)](https://travis-ci.org/alexmurray/flycheck-posframe)

Display flycheck error messages via
[posframe](https://github.com/tumashu/posframe).

![flycheck-posframe screenshot](screenshots/flycheck-posframe.png)

## Installation

### MELPA

The preferred way to install `flycheck-posframe` is via
[MELPA](http://melpa.org) - then you can just <kbd>M-x package-install RET
flycheck-posframe RET</kbd>

To enable then simply add the following to your init file:

```emacs-lisp
(with-eval-after-load 'flycheck
  (require 'flycheck-posframe)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
```

We recommend to use [use-package](https://github.com/jwiegley/use-package) to
make this automatic:

```emacs-lisp
(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
```

### Manual

If you would like to install the package manually, download or clone it and
place within Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'flycheck-posframe)
(add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
```

NOTE: This will also require the manual installation of `flycheck` if you have
not done so already.

## Configuration

### Default Pretty Configuration

Calling `(flycheck-posframe-configure-pretty-defaults)` will configure
`flycheck-posframe` to show warnings and errors with nicer faces (inheriting
from `warning` and `error` respectively), and set the prefix for each to nicer
unicode characters.

### Manual Configuration

There are a few settings which can be configured to customise the display of
error messages. These include the faces which are used to show the error
messages `flycheck-posframe-info-face`, `flycheck-posframe-warning-face` and
`flycheck-posframe-error-face` and a string which is used as the prefix for
each message `flycheck-posframe-info-prefix``flycheck-posframe-warning-prefix`
and `flycheck-posframe-error-prefix`.

#### `flycheck-posframe-LEVEL-face`

By default each of `flycheck-posframe-LEVEL-face` inherits from the `default`
face so should appear like the rest of the text in the buffer. If however you
want to ensure error messages stand-out you could configure them to inherit
from the `error` face:

```
(set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error)
```

#### `flycheck-posframe-LEVEL-prefix`

By default `flycheck-posframe-LEVEL-prefix` is set to "➤ ". If however you
wanted to show each warning message prefixed with the unicode WARNING SIGN symbol
(U+26A0) "⚠ " you could configure it as follows:

```
(setq flycheck-posframe-warning-prefix "\u26a0 ")
```

## License

Copyright © 2018 Alex Murray

Distributed under GNU GPL, version 3.
