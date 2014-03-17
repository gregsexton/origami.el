;;; origami.el --- Flexible text folding  -*- lexical-binding: t -*-

;; Author: Greg Sexton <gregsexton@gmail.com>
;; Version: 1.0
;; Keywords: folding
;; URL: https://github.com/gregsexton/
;; Package-Requires: ((emacs "24"))

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

;;; customisation

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

(defun origami-fold-open-set (node value)
  (origami-fold-node (origami-fold-beg node)
                     (origami-fold-end node)
                     value
                     (origami-fold-children node)
                     (origami-fold-data node)))

(defun origami-fold-open-toggle (node)
  (origami-fold-open-set node (not (origami-fold-open-p node))))

(defun origami-fold-shallow-equal (a b)
  (and (equal (origami-fold-beg a) (origami-fold-beg b))
       (equal (origami-fold-end a) (origami-fold-end b))
       (equal (origami-fold-open-p a) (origami-fold-open-p b))))

(defun origami-fold-diff (old new on-add on-remove)
  "Diff the two structures calling ON-ADD for nodes that have
been added and ON-REMOVE for nodes that have been removed."
  (cl-labels ((pair-off (old-children new-children)
                        (let ((old (car old-children))
                              (new (car new-children)))
                          (cond ((null old) (-map (lambda (n) (cons nil n)) new-children))
                                ((null new) (-map (lambda (n) (cons n nil)) old-children))
                                ((and (null old) (null new)) '())
                                ((origami-fold-shallow-equal old new) (cons (cons old new)
                                                                            (pair-off (cdr old-children)
                                                                                      (cdr new-children))))
                                ((<= (origami-fold-beg old)
                                     (origami-fold-beg new)) (cons (cons old nil)
                                                                   (pair-off (cdr old-children)
                                                                             new-children)))
                                (t (cons (cons nil new)
                                         (pair-off old-children
                                                   (cdr new-children)))))))
              (handle-pair (pair)
                           (let ((old (car pair))
                                 (new (cdr pair)))
                             (cond ((and old new) (origami-fold-diff old new on-add on-remove))
                                   (old (funcall on-remove old))
                                   (new (funcall on-add new))
                                   t (error "Illegal pairing.")))))
    (unless (origami-fold-shallow-equal old new)
      (error "Precondition invalid: old must be shallow-equal to new."))
    (-each (pair-off (origami-fold-children old)
                     (origami-fold-children new))
           (lambda (pair) (handle-pair pair)))))

(defun origami-fold-postorder-each (node f)
  (-each (origami-fold-children node) f)
  (funcall f node))

;;; overlay manipulation

(defun origami-create-fold-overlay (beg end buffer text)
  (let ((ov (make-overlay beg end buffer)))
    (overlay-put ov 'invisible 'origami)
    ;; TODO: make this customizable
    (overlay-put ov 'display text)
    (overlay-put ov 'face 'font-lock-comment-delimiter-face)
    ov))

(defun origami-create-overlay-from-fold-fn (buffer)
  (lambda (node)
    (origami-fold-postorder-each
     node (lambda (n)
            (let ((overlay (origami-create-fold-overlay (origami-fold-beg n)
                                                        (origami-fold-end n) buffer "...")))
              (origami-fold-data n overlay))))))

(defun origami-delete-overlay-from-fold-fn (buffer)
  (lambda (node)
    (origami-fold-postorder-each
     node (lambda (node)
            (-when-let (ov (origami-fold-data node))
              (delete-overlay ov))))))

(defun origami-remove-all-overlays (buffer)
  ;; TODO:
  )

;;; commands

(defun origami-reset (buffer)
  ;; TODO: provide this to the user in case we get screwed up, maybe
  ;; use this when disabling the minor mode?
  (interactive)
  (origami-remove-all-overlays buffer)
  ;; TODO: remove fold ds
  )

;;; origami.el ends here
