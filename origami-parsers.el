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
(require 'cl)

(defcustom origami-parser-alist
  '((java-mode             . origami-c-style-parser)
    (c-mode                . origami-c-style-parser)
    (c++-mode              . origami-c-style-parser)
    (perl-mode             . origami-c-style-parser)
    (cperl-mode            . origami-c-style-parser)
    (js-mode               . origami-c-style-parser)
    (js2-mode              . origami-c-style-parser)
    (js3-mode              . origami-c-style-parser)
    (emacs-lisp-mode       . origami-elisp-parser)
    (lisp-interaction-mode . origami-elisp-parser)
    (clojure-mode          . origami-clj-parser))
  "alist mapping major-mode to parser function."
  :type 'hook
  :group 'origami)

(defun origami-get-positions (content regex)
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let (acc)
      (while (re-search-forward regex nil t)
        (let ((match (match-string 0)))
          (setq acc (cons (cons match (- (point) (length match)))
                          acc))))
      (reverse acc))))

(defun origami-build-pair-tree (create open close positions)
  (cl-labels ((build (positions)
                     ;; this is so horrible, but fast
                     (let (acc beg (should-continue t))
                       (while (and should-continue positions)
                         (cond ((equal (caar positions) open)
                                (if beg                       ;go down a level
                                    (let* ((res (build positions))
                                           (new-pos (car res))
                                           (children (cdr res)))
                                      (setq positions (cdr new-pos))
                                      (setq acc (cons (funcall create beg (cdar new-pos) (length open) children)
                                                      acc))
                                      (setq beg nil))
                                  ;; begin a new pair
                                  (setq beg (cdar positions))
                                  (setq positions (cdr positions))))
                               ((equal (caar positions) close)
                                (if beg
                                    (progn                 ;close with no children
                                      (setq acc (cons (funcall create beg (cdar positions) (length close) nil)
                                                      acc))
                                      (setq positions (cdr positions))
                                      (setq beg nil))
                                  (setq should-continue nil)))))
                       (cons positions (reverse acc)))))
    (cdr (build positions))))

(defun origami-c-style-parser (create)
  (lambda (content)
    (let ((positions (origami-get-positions content "[{}]")))
      (origami-build-pair-tree create "{" "}" positions))))

(defun origami-lisp-parser (create regex)
  (lambda (content)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (beginning-of-defun -1)
      (let (beg end offset acc)
        (while (< (point) (point-max))
          (setq beg (point))
          (search-forward-regexp regex nil t)
          (setq offset (- (point) beg))
          (end-of-defun)
          (backward-char)               ;move point to one after the last paren
          (setq end (1- (point)))       ;don't include the last paren in the fold
          (when (> offset 0)
            (setq acc (cons (funcall create beg end offset nil) acc)))
          (beginning-of-defun -1))
        (reverse acc)))))

(defun origami-elisp-parser (create)
  (origami-lisp-parser create "(def\\w*\\s-*\\(\\s_\\|\\w\\|[?!]\\)*\\([ \\t]*(.*?)\\)?"))

(defun origami-clj-parser (create)
  (origami-lisp-parser create "(def\\(\\w\\|-\\)*\\s-*\\(\\s_\\|\\w\\|[?!]\\)*\\([ \\t]*\\[.*?\\]\\)?"))

(provide 'origami-parsers)

;;; origami-parsers.el ends here
