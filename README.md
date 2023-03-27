# Colors

[![MELPA Stable](https://stable.melpa.org/packages/colors-badge.svg)](https://stable.melpa.org/#/colors)
[![MELPA](https://melpa.org/packages/colors-badge.svg)](https://melpa.org/#/colors)

## Introduction
This repository contains `colors`, a small Emacs Lisp package providing
various functions to manipulate colors.

While Emacs contains a builtin `color` package, it has several shortcomings:

- Functions are hard to combine because colors are usually returned as lists
  but passed as separate arguments.
- The representation of RGB color components as floating point values between
  0.0 and 1.0 is unusual and annoying to work with.
- There is no support for HSL color strings.
- Results are not what you would expect. For example converting `#cc241d` to
  HSL yields a hue of 0.006666666666666636 (e.g. 0°) instead of 2°.
  
The `colors` package aims to be easier to use.

## Licensing
The `colors` package is open source software distributed under the
[ISC](https://opensource.org/licenses/ISC) license.

## Contact
If you have an idea or a question, feel free to email me at
<nicolas@n16f.net>.
