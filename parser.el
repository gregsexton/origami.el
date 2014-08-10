;;; parser.el --- Monadic parser combinator  -*- lexical-binding: t -*-

;; Author: Greg Sexton <gregsexton@gmail.com>
;; Version: 1.0
;; Keywords: parser
;; URL: https://github.com/gregsexton/
;; Package-Requires: ((s "1.9.0") (dash "2.5.0") (emacs "24"))

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

(require 'dash)
(require 's)
(require 'cl)

;;; content structure

(defun parser-content (consumed string)
  "Create a content structure from STRING and the count of CONSUMED characters."
  (cons consumed string))

(defun parser-content-consumed-count (content) (car content))

(defun parser-content-string (content) (cdr content))

(defun parser-content-from (content consumed)
  "Create a new content after consuming CONSUMED chars."
  (parser-content (+ (parser-content-consumed-count content) consumed)
                  (substring (parser-content-string content) consumed)))

;;; parsers
;;; TODO: document parsers

(defun parser-run (parser content)
  (funcall parser content))

(defun parser-bind (m f)
  "State monad composed with the maybe monad."
  (if (null m) nil
    (lambda (s)
      (let ((new-result (parser-run m s)))
        (if (null new-result) nil
          (destructuring-bind (new-value . new-state) new-result
            (parser-run (funcall f new-value) new-state)))))))

(defmacro parser-do (expr &rest more)
  (let ((assignment-p (and (listp expr) (equal (cadr expr) '<-))))
    (let ((var (if assignment-p (car expr) (gensym)))
          (f (if assignment-p (caddr expr) expr)))
      (if more
          `(parser-bind
            ,f (lambda (,var)
                 (parser-do ,@more)))
        f))))

(defun parser-return (x)
  (lambda (s) (cons x s)))

(defun parser-zero ()
  (lambda (s) nil))

(defun parser-get ()
  (lambda (s) (cons s s)))

(defun parser-put (x)
  (lambda (s) (cons nil x)))

(defun parser-get-string ()
  (parser-do (content <- (parser-get))
             (parser-return (parser-content-string content))))

(defun parser-drop (n)
  (parser-do (content <- (parser-get))
             (parser-put (parser-content-from content n))
             ;; TODO: substring will error if n is too large, guard against this
             (parser-return n)))

(defun parser-take (n)
  (lambda (content)
    (let ((content-str (parser-content-string content)))
      (unless (s-blank? content-str)
        (let* ((len (length content-str))
               (n (if (> n len) len n)))
          (cons (substring content-str 0 n) (parser-content-from content n)))))))

(defun parser-item ()
  (parser-take 1))

(defun parser-position ()
  "Returns the point position, which is 1 more than the current
consumed count."
  (parser-do (content <- (parser-get))
             (parser-return (+ (parser-content-consumed-count content) 1))))

(defun parser-sat (pred)
  (parser-do (pos <- (parser-position))
             (a <- (parser-item))
             (if (funcall pred a)
                 (parser-return (+ pos 1))
               (parser-zero))))

(defun parser-char (x)
  (parser-sat (lambda (c) (equal x c))))

(defun parser-string (str)
  ;; take rather than recursion due to elisp
  (parser-do (prefix <- (parser-take (length str)))
             (pos <- (parser-position))
             (if (equal str prefix)
                 (parser-return pos)
               (parser-zero))))

(defun parser-regex (rx)
  "Match the regex somewhere in the remaining string. Note you
have to prefix with '^' if you wish to match the beginning."
  (parser-do (str <- (parser-get-string))
             (if (string-match rx str)
                 (parser-drop (match-end 0))
               (parser-zero))
             (parser-position)))

;;; TODO: rename? parser-consume-while-not ?
(defun parser-drop-until-regex (rx)
  "Skip over all characters until hitting RX. If RX is not found
this will bind to zero. If RX is matched at the beginning of the
string, we bind to zero. This allows for bottoming out of
recursion. We fail if we don't consume something."
  (parser-do (str <- (parser-get-string))
             (if (string-match rx str)
                 (if (> (match-beginning 0) 0)
                     (parser-drop (match-beginning 0))
                   (parser-zero))
               (parser-zero))))

(defun parser-conj (p1 p2)
  (lambda (content)
    (or (parser-run p1 content)
        (parser-run p2 content))))

(defun parser-0+ (p)
  (parser-conj
   (parser-1+ p)
   (parser-return nil)))

(defun parser-1+ (p)
  ;; recursive isn't going to cut it in elisp
  (lambda (content)
    (let ((res (parser-run p content))
          (acc nil))
      (while res
        (setq acc (cons (car res) acc))
        (setq content (cdr res))
        (setq res (parser-run p content)))
      (when acc
        (cons (reverse acc) content)))))

(defun parser-1? (p)
  (parser-conj p (parser-return nil)))

(defun parser-not (parser)
  (lambda (content)
    (if (parser-run parser content)
        nil
      (parser-run (parser-item) content))))

(provide 'parser)

;;; parser.el ends here
