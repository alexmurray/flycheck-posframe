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

## License

Copyright © 2018 Alex Murray

Distributed under GNU GPL, version 3.
