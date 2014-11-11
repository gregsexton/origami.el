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
    (cperl-mode            . origami-c-style-parser)
    (emacs-lisp-mode       . origami-elisp-parser)
    (lisp-interaction-mode . origami-elisp-parser)
    (clojure-mode          . origami-clj-parser))
  "alist mapping major-mode to parser function."
  :type 'hook
  :group 'origami)

;;; TODO: generalize to take a function? then could use a regex or
;;; begin-defun etc?
(defun origami-get-positions (content regex)
  (with-temp-buffer
    (insert content)
    (beginning-of-buffer)
    (let (acc)
      (while (re-search-forward regex nil t)
        (setq acc (cons (cons (match-string 0) (point)) acc)))
      (reverse acc))))

(defun origami-build-pair-tree (create positions)
  ;; this is so horrible, but fast
  (let (acc beg (should-continue t))
    (while (and should-continue positions)
      (cond ((equal (caar positions) "{")
             (if beg                       ;go down a level
                 (let* ((res (origami-build-pair-tree create positions))
                        (new-pos (car res))
                        (children (cdr res)))
                   (setq positions (cdr new-pos))
                   (setq acc (cons (funcall create beg (cdar new-pos) 0 children) acc))
                   (setq beg nil))
               ;; begin a new pair
               (setq beg (cdar positions))
               (setq positions (cdr positions))))
            ((equal (caar positions) "}")
             (if beg
                 (progn                 ;close no children
                   (setq acc (cons (funcall create beg (cdar positions) 0 nil) acc))
                   (setq positions (cdr positions))
                   (setq beg nil))
               (setq should-continue nil)))))
    (cons positions (reverse acc))))

(defun origami-c-style-parser (create)
  (lambda (content)
    (let ((positions (origami-get-positions (parser-content-string content) "[{}]")))
      (list (cdr (origami-build-pair-tree create positions))))))

(defun origami-lisp-parser (create regex)
  (lambda (content)
    (with-temp-buffer
      (insert (parser-content-string content))
      (beginning-of-buffer)
      (beginning-of-defun -1)
      (let (beg end offset acc)
        (while (< (point) (point-max))
          (setq beg (point))
          (search-forward-regexp regex nil t)
          (setq offset (- (point) beg))
          (end-of-defun)
          (backward-char)
          (setq end (point))
          (when (> offset 0)
            (setq acc (cons (funcall create beg end offset nil) acc)))
          (beginning-of-defun -1))
        (list (reverse acc))))))

(defun origami-elisp-parser (create)
  (origami-lisp-parser create "(def\\w*\\s-*\\(\\s_\\|\\w\\|[?!]\\)*\\([ \\t]*(.*?)\\)?"))

(defun origami-clj-parser (create)
  (origami-lisp-parser create "(def\\(\\w\\|-\\)*\\s-*\\(\\s_\\|\\w\\|[?!]\\)*\\([ \\t]*\\[.*?\\]\\)?"))

(provide 'origami-parsers)

;;; parser.el ends here
