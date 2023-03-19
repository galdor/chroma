;;; colors-test.el --- Tests for the colors package -*- lexical-binding: t -*-

;; Author: Nicolas Martyanoff <nicolas@n16f.net>
;; URL: https://github.com/galdor/colors
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))

;; Copyright 2023 Nicolas Martyanoff <nicolas@n16f.net>
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
;; IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; This package contains unit tests for the colors package.

;;; Code:

(require 'colors)
(require 'ert)

(ert-deftest colors-parse-rgb ()
  (should (equal (colors-parse-rgb "#000000") '(0 0 0)))
  (should (equal (colors-parse-rgb "#102040") '(16 32 64)))
  (should (equal (colors-parse-rgb "#ffffff") '(255 255 255)))
  (should (equal (colors-parse-rgb "123456") nil)))

(ert-deftest colors-format-rgb ()
  (should (equal (colors-format-rgb '(0 0 0)) "#000000"))
  (should (equal (colors-format-rgb '(16 32 64)) "#102040"))
  (should (equal (colors-format-rgb '(255 255 255)) "#ffffff")))

(ert-deftest colors-parse-hsl ()
  (should (equal (colors-parse-hsl "hsl(0,0%,0%)") '(0 0.0 0.0)))
  (should (equal (colors-parse-hsl "hsl(0, 50%, 100%)") '(0 0.5 1.0)))
  (should (equal (colors-parse-hsl "hsl(360, 20%, 40%)") '(360 0.2 0.4)))
  (should (equal (colors-parse-hsl "hsl(420, 20%, 40%)") nil))
  (should (equal (colors-parse-hsl "hsl(180, 200%, 0%)") nil))
  (should (equal (colors-parse-hsl "hsl(180, 10%, 150%)") nil))
  (should (equal (colors-parse-hsl "1, 2, 3") nil)))

(ert-deftest colors-format-hsl ()
  (should (equal (colors-format-hsl '(0 0.0 0.0)) "hsl(0, 0%, 0%)"))
  (should (equal (colors-format-hsl '(360 1.0 1.0)) "hsl(360, 100%, 100%)"))
  (should (equal (colors-format-hsl '(180 0.2 0.4)) "hsl(180, 20%, 40%)")))

(ert-deftest colors-rgb-to-hsl ()
  (should (equal (colors-rgb-to-hsl '(  0   0   0)) '(  0 0.0 0.00)))
  (should (equal (colors-rgb-to-hsl '(255 255 255)) '(  0 0.0 1.00)))
  (should (equal (colors-rgb-to-hsl '(255   0   0)) '(  0 1.0 0.50)))
  (should (equal (colors-rgb-to-hsl '(  0 255   0)) '(120 1.0 0.50)))
  (should (equal (colors-rgb-to-hsl '(  0   0 255)) '(240 1.0 0.50)))
  (should (equal (colors-rgb-to-hsl '(255 255   0)) '( 60 1.0 0.50)))
  (should (equal (colors-rgb-to-hsl '(  0 255 255)) '(180 1.0 0.50)))
  (should (equal (colors-rgb-to-hsl '(255   0 255)) '(300 1.0 0.50)))
  (should (equal (colors-rgb-to-hsl '(191 191 191)) '(  0 0.0 0.75)))
  (should (equal (colors-rgb-to-hsl '(128 128 128)) '(  0 0.0 0.50)))
  (should (equal (colors-rgb-to-hsl '(128   0   0)) '(  0 1.0 0.25)))
  (should (equal (colors-rgb-to-hsl '(128 128   0)) '( 60 1.0 0.25)))
  (should (equal (colors-rgb-to-hsl '(  0 128   0)) '(120 1.0 0.25)))
  (should (equal (colors-rgb-to-hsl '(128   0 128)) '(300 1.0 0.25)))
  (should (equal (colors-rgb-to-hsl '(  0 128 128)) '(180 1.0 0.25)))
  (should (equal (colors-rgb-to-hsl '(  0   0 128)) '(240 1.0 0.25))))

(ert-deftest colors-hsl-to-rgb ()
  (should (equal (colors-hsl-to-rgb '(  0 0.0 0.00)) '(  0   0   0)))
  (should (equal (colors-hsl-to-rgb '(  0 0.0 1.00)) '(255 255 255)))
  (should (equal (colors-hsl-to-rgb '(  0 1.0 0.50)) '(255   0   0)))
  (should (equal (colors-hsl-to-rgb '(120 1.0 0.50)) '(  0 255   0)))
  (should (equal (colors-hsl-to-rgb '(240 1.0 0.50)) '(  0   0 255)))
  (should (equal (colors-hsl-to-rgb '( 60 1.0 0.50)) '(255 255   0)))
  (should (equal (colors-hsl-to-rgb '(180 1.0 0.50)) '(  0 255 255)))
  (should (equal (colors-hsl-to-rgb '(300 1.0 0.50)) '(255   0 255)))
  (should (equal (colors-hsl-to-rgb '(  0 0.0 0.75)) '(191 191 191)))
  (should (equal (colors-hsl-to-rgb '(  0 0.0 0.50)) '(128 128 128)))
  (should (equal (colors-hsl-to-rgb '(  0 1.0 0.25)) '(128   0   0)))
  (should (equal (colors-hsl-to-rgb '( 60 1.0 0.25)) '(128 128   0)))
  (should (equal (colors-hsl-to-rgb '(120 1.0 0.25)) '(  0 128   0)))
  (should (equal (colors-hsl-to-rgb '(300 1.0 0.25)) '(128   0 128)))
  (should (equal (colors-hsl-to-rgb '(180 1.0 0.25)) '(  0 128 128)))
  (should (equal (colors-hsl-to-rgb '(240 1.0 0.25)) '(  0   0 128))))

(provide 'colors-test)

;;; colors-test.el ends here
