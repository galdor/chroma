;;; colors.el --- Color manipulation library -*- lexical-binding: t -*-

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

;; This package provides utilities to manipulate colors. Compared to the
;; built-in color.el, colors.el uses provides a consistent interface, always
;; accepting and returning colors as lists. It also uses commonly used
;; representation for components: 8 bit values for RGB colors, 0-360 hue for
;; HSL colors.
;;
;; RGB colors are represented as lists of three elements for the red, green
;; and blue components. RGB components are represented as 8 bit integer.
;;
;; HSL colors are represented as lists of three elements for the hue,
;; saturation and lightness. Hue is represented as an integer in [0, 360].
;; Saturation and lightness are represented as floating point numbers in [0.0,
;; 1.0].

;;; Code:

(defun colors-parse-rgb (string)
  "Parse an hexadecimal RGB color string (e.g. \"#204080\").
Return the three RGB components as a list or NIL if STRING does
not represent an RGB color."
  (when (string-match "^#\\([0-9A-Za-z]\\{6\\}\\)$" string)
    (let* ((digits (match-string 1 string))
           (r (string-to-number (substring digits 0 2) 16))
           (g (string-to-number (substring digits 2 4) 16))
           (b (string-to-number (substring digits 4 6) 16)))
      (list r g b))))

(defun colors-format-rgb (color)
  "Return the hexadecimal representation of an RGB color."
  (let ((r (nth 0 color))
        (g (nth 1 color))
        (b (nth 2 color)))
    (format "#%02x%02x%02x" r g b)))

(defun colors-parse-hsl (string)
  "Parse an HSL color string (e.g. \"hsl(180, 20%, 40%)\").
Return the three HSL components as a list or NIL if STRING does
not represent a HSL color."
  (when (string-match
         "^hsl( *\\([0-9]+\\) *, *\\([0-9]+\\)% *, *\\([0-9]+\\)% *)$" string)
    (let* ((h (string-to-number (match-string 1 string)))
           (s (/ (string-to-number (match-string 2 string)) 100.0))
           (l (/ (string-to-number (match-string 3 string)) 100.0)))
      (when (and (<= 0 h 360)
                 (<= 0 s 100)
                 (<= 0 l 100))
        (list h s l)))))

(defun colors-format-hsl (color)
  "Return the representation of a HSL color."
  (let ((h (nth 0 color))
        (s (nth 1 color))
        (l (nth 2 color)))
    (format "hsl(%d, %.0f%%, %.0f%%)" h (* s 100.0) (* l 100.0))))

(provide 'colors)

;;; colors.el ends here
