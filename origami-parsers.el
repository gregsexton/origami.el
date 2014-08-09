;;; origami-parsers.el --- Collection of parsers  -*- lexical-binding: t -*-

;; Author: Greg Sexton <gregsexton@gmail.com>
;; Version: 1.0
;; Keywords: parsers
;; URL: https://github.com/gregsexton/

;; The MIT License (MIT)

;; Copyright (c) 2014 Greg Sexton

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;;; Code:

(require 'parser)

(defcustom origami-parser-alist
  '((java-mode             . origami-c-style-parser)
    (c-mode                . origami-c-style-parser)
    (c++-mode              . origami-c-style-parser)
    (emacs-lisp-mode       . origami-elisp-parser)
    (lisp-interaction-mode . origami-elisp-parser))
  "alist mapping major-mode to parser function."
  :type 'hook
  :group 'origami)

(defun origami-c-style-parser (create)
  (let ((pair (parser-paired (parser-char "{")
                             (lambda () (origami-c-style-parser create))
                             (parser-char "}")
                             create)))
    (parser-0+ (parser-conj
                (parser-do
                 (parser-drop-until-regex "[{}]")
                 (parser-1? pair))
                pair))))

(defun origami-elisp-parser (create)
  (let ((pair (parser-paired (parser-char "(")
                             (lambda () (origami-elisp-parser create))
                             (parser-char ")")
                             create)))
    (parser-0+ (parser-conj
                (parser-do
                 (parser-drop-until-regex "[()]")
                 (parser-1? pair))
                pair))))

(provide 'origami-parsers)

;;; parser.el ends here
