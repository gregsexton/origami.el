;;; origami.el --- Flexible text folding  -*- lexical-binding: t -*-

;; Author: Greg Sexton <gregsexton@gmail.com>
;; Version: 1.0
;; Keywords: folding
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

;;; fold structure

(defun origami-fold-node (beg end open &optional children data)
  ;; TODO: ensure invariant: sort children and ensure that none
  ;; overlap
  (vector beg end open children data))

(defun origami-top-level-node (&optional children)
  (origami-fold-node 0 0 t children))

(defun origami-fold-beg (node) (aref node 0))

(defun origami-fold-end (node) (aref node 1))

(defun origami-fold-open-p (node) (aref node 2))

(defun origami-fold-children (node) (aref node 3))

(defun origami-fold-data (node &optional data)
  "With optional param DATA, add or replace data. This cannot be
used to nil out data. This mutates the node."
  (if data
      (aset node 4 data)
    (aref node 4)))

;;; need to rewrite the fold tree structure from the node downwards
;; (defun origami-fold-open-set (node value)
;;   (origami-fold-node (origami-fold-beg node)
;;                      (origami-fold-end node)
;;                      value
;;                      (origami-fold-children node)
;;                      (origami-fold-data node)))

;; (defun origami-fold-open-toggle (node)
;;   (origami-fold-open-set node (not (origami-fold-open-p node))))

(defun origami-fold-range-equal (a b)
  (and (equal (origami-fold-beg a) (origami-fold-beg b))
       (equal (origami-fold-end a) (origami-fold-end b))))

(defun origami-fold-state-equal (a b)
  (equal (origami-fold-open-p a) (origami-fold-open-p b)))

(defun origami-fold-diff (old new on-add on-remove on-change)
  (cl-labels ((diff-children (old-children new-children)
                             (let ((old (car old-children))
                                   (new (car new-children)))
                               (cond ((null old) (-each new-children on-add))
                                     ((null new) (-each old-children on-remove))
                                     ((and (null old) (null new)) nil)
                                     ((origami-fold-range-equal old new)
                                      (origami-fold-diff old new on-add on-remove on-change)
                                      (diff-children (cdr old-children) (cdr new-children)))
                                     ((<= (origami-fold-beg old) (origami-fold-beg new))
                                      (funcall on-remove old)
                                      (diff-children (cdr old-children) new-children))
                                     (t (funcall on-add new)
                                        (diff-children old-children (cdr new-children)))))))
    (unless (origami-fold-range-equal old new)
      (error "Precondition invalid: old must have the same range as new."))
    (unless (origami-fold-state-equal old new)
      (funcall on-change old new))
    (diff-children (origami-fold-children old)
                   (origami-fold-children new))))

(defun origami-fold-postorder-each (node f)
  (-each (origami-fold-children node) f)
  (funcall f node))

;;; overlay manipulation

(defun origami-create-overlay (beg end buffer text)
  (let ((ov (make-overlay beg end buffer)))
    (overlay-put ov 'invisible 'origami)
    ;; TODO: make this customizable
    (overlay-put ov 'display text)
    (overlay-put ov 'face 'font-lock-comment-delimiter-face)
    ov))

(defun origami-create-overlay-for-node (node buffer)
  (let ((overlay (origami-create-overlay (origami-fold-beg node)
                                         (origami-fold-end node) buffer "...")))
    (origami-fold-data node overlay)))

(defun origami-create-overlay-from-fold-tree-fn (buffer)
  (lambda (node)
    (origami-fold-postorder-each
     node (lambda (n)
            (when (not (origami-fold-open n))
              (origami-create-overlay-for-node n buffer))))))

(defun origami-delete-overlay-from-fold-tree-fn (buffer)
  (lambda (node)
    (origami-fold-postorder-each
     node (lambda (node)
            (-when-let (ov (origami-fold-data node))
              (delete-overlay ov))))))

(defun origami-change-overlay-from-fold-node-fn (buffer)
  (lambda (old new)
    (if (origami-fold-open-p new)
        (delete-overlay (origami-fold-data old))
      (origami-create-overlay-for-node new buffer))))

(defun origami-remove-all-overlays (buffer)
  ;; TODO:
  )

;;; content structure

(defun origami-content (consumed string)
  "Create a content structure from STRING and the count of CONSUMED characters."
  (cons consumed string))

(defun origami-content-consumed-count (content) (car content))

(defun origami-content-string (content) (cdr content))

(defun origami-content-from (content consumed)
  "Create a new content after consuming CONSUMED chars."
  (origami-content (+ (origami-content-consumed-count content) consumed)
                   (substring (origami-content-string content) consumed)))

;;; scanner

(defun origami-scanner-bind (h f)
  "State monad composed with the maybe monad."
  (if (null h) nil
    (lambda (s)
      (let ((new-result (funcall h s)))
        (if (null new-result) nil
          (destructuring-bind (new-value . new-state) new-result
            (funcall (funcall f new-value) new-state)))))))

;;; TODO: reimplement this as a macro so that intermediate results are
;;; visible. Basically implement proper do notation.
(defun origami-scanner-bind-chain (scanner &rest funs)
  (-reduce-from (lambda (acc f) (origami-scanner-bind acc f)) scanner funs))

(defun origami-scanner-return (x)
  (lambda (s) (cons x s)))

(defun origami-run-scanner (scanner content)
  (funcall scanner content))

(defun origami-scan (buffer scanner)
  (with-current-buffer buffer
    (let ((contents (buffer-string)))
      (origami-run-scanner scanner (origami-content 0 contents)))))

(defun origami-scanner-consume (prefix content)
  (lambda (state)
    (when (s-prefix-p prefix (origami-content-string content))
      (cons (origami-content-from content (length prefix)) state))))

(defun origami-pair-scanner (start end skip)
  (lambda (content)
    (origami-scanner-bind-chain
     (origami-scanner-consume start content)
     ;; skip
     (lambda (skip-content) (origami-scanner-consume end skip-content))
     (lambda (new-content)
       (lambda (state)
         (cons new-content (cons (list (origami-content-consumed-count content)
                                       (origami-content-consumed-count new-content))
                                 state)))))))

;;; TODO: maybe scan and build up a simple list of (start end) values,
;;; in the next pass transform this in to the proper fold structure?

(origami-run-scanner
 (origami-scanner-bind
  (origami-scanner-return (origami-content 0 "{}"))
  (origami-pair-scanner "{" "}" nil))
 'foo) ;=> ((2 . "") . foo)

;;; commands

(defun origami-reset (buffer)
  ;; TODO: provide this to the user in case we get screwed up, maybe
  ;; use this when disabling the minor mode?
  (interactive)
  (origami-remove-all-overlays buffer)
  ;; TODO: remove fold ds
  )

;;; origami.el ends here
