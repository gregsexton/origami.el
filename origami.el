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
  (let ((sorted-children (-sort (lambda (a b)
                                  (or (< (origami-fold-beg a) (origami-fold-beg b))
                                      (and (= (origami-fold-beg a) (origami-fold-beg b))
                                           (< (origami-fold-end a) (origami-fold-end b)))))
                                (remove nil children))))
    ;; ensure invariant: no children overlap
    (when (-some? (lambda (pair)
                    (let ((a (car pair))
                          (b (cadr pair)))
                      (when b ;for the odd numbered case - there may be a single item
                        ;; the < function doesn't support varargs
                        (or (>= (origami-fold-beg a) (origami-fold-end a))
                            (>= (origami-fold-end a) (origami-fold-beg b))
                            (>= (origami-fold-beg b) (origami-fold-end b))))))
                  (-partition-all-in-steps 2 1 sorted-children))
      (error "Tried to construct a node where the children overlap or are not distinct regions: %s"
             sorted-children))
    ;; ensure invariant: parent encompases children
    (let ((beg-children (origami-fold-beg (car sorted-children)))
          (end-children (origami-fold-end (-last-item sorted-children))))
      (if (and beg-children (or (> beg beg-children) (< end end-children)))
          (error "Node does not overlap children in range. beg=%s end=%s beg-children=%s end-children=%s"
               beg end beg-children end-children)
        (vector beg end open sorted-children data)))))

(defun origami-fold-top-level-node (&optional children)
  ;; TODO: fix min and max
  (origami-fold-node 1 10000 t children))

(defun origami-fold-beg (node) (when node (aref node 0)))

(defun origami-fold-end (node) (when node (aref node 1)))

(defun origami-fold-open-p (node) (when node (aref node 2)))

(defun origami-fold-open-set (path value)
  (let* ((old-node (-last-item path))
         (new-node (origami-fold-node (origami-fold-beg old-node)
                                      (origami-fold-end old-node)
                                      value
                                      (origami-fold-children old-node)
                                      (origami-fold-data old-node))))
    (origami-fold-assoc path new-node)))

(defun origami-fold-children (node &optional children)
  (when node
    (if children
        (origami-fold-node (origami-fold-beg node)
                           (origami-fold-end node)
                           (origami-fold-open-p node)
                           children
                           (origami-fold-data node))
      (aref node 3))))

(defun origami-fold-data (node &optional data)
  "With optional param DATA, add or replace data. This cannot be
used to nil out data. This mutates the node."
  (when node
    (if data
        (aset node 4 data)
      (aref node 4))))

(defun origami-fold-range-equal (a b)
  (and (equal (origami-fold-beg a) (origami-fold-beg b))
       (equal (origami-fold-end a) (origami-fold-end b))))

(defun origami-fold-state-equal (a b)
  (equal (origami-fold-open-p a) (origami-fold-open-p b)))

(defun origami-fold-replace-child (node old new)
  (origami-fold-children node
                         (cons new (remove old (origami-fold-children node)))))

(defun origami-fold-assoc (path new-node)
  "Rewrite the tree, replacing the node referenced by path with NEW-NODE"
  (cdr
   (-reduce-r-from (lambda (node acc)
                     (destructuring-bind (old-node . new-node) acc
                       (cons node (origami-fold-replace-child node old-node new-node))))
                   (cons (-last-item path) new-node)
                   (butlast path))))

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

(defun origami-fold-find-deepest (tree pred)
  (when tree
    (when (funcall pred tree)
      (-if-let (child (-first pred (origami-fold-children tree)))
          (cons tree (origami-fold-find-deepest child pred))
        (list tree)))))

(defun origami-fold-find-node-with-range (tree beg end)
  "Return the path to the most specific (deepest) node that has
exactly the range BEG-END, or null."
  (-when-let (path (origami-fold-find-deepest tree
                                              (lambda (node)
                                                (and (>= beg (origami-fold-beg node))
                                                     (<= end (origami-fold-end node))))))
    (let ((last (-last-item path)))
      (when (and (= beg (origami-fold-beg last))
                 (= end (origami-fold-end last)))
        path))))

(defun origami-fold-find-node-containing (tree point)
  "Return the path to the most specific (deepest) node that
contains point, or null."
  (origami-fold-find-deepest tree
                             (lambda (node)
                               (and (<= (origami-fold-beg node) point)
                                    (>= (origami-fold-end node) point)))))

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

;;; monadic parser combinator

(defun origami-run-parser (parser content)
  (funcall parser content))

(defun origami-parser-bind (m f)
  "State monad composed with the maybe monad."
  (if (null m) nil
    (lambda (s)
      (let ((new-result (origami-run-parser m s)))
        (if (null new-result) nil
          (destructuring-bind (new-value . new-state) new-result
            (origami-run-parser (funcall f new-value) new-state)))))))

(defmacro origami-do (expr &rest more)
  (let ((assignment-p (and (listp expr) (equal (cadr expr) '<-))))
    (let ((var (if assignment-p (car expr) (gensym)))
          (f (if assignment-p (caddr expr) expr)))
      (if more
          `(origami-parser-bind
            ,f (lambda (,var)
                 (origami-do ,@more)))
        f))))

(defun origami-parser-return (x)
  (lambda (s) (cons x s)))

(defun origami-parser-zero ()
  (lambda (s) nil))

(defun origami-parser-get ()
  (lambda (s) (cons s s)))

(defun origami-parser-item ()
  (lambda (content)
    (let ((content-str (origami-content-string content)))
      (unless (s-blank? content-str)
        (cons (substring content-str 0 1) (origami-content-from content 1))))))

(defun origami-parser-position ()
  "Returns the point position, which is 1 more than the current
consumed count."
  (origami-do (content <- (origami-parser-get))
              (origami-parser-return (+ (origami-content-consumed-count content) 1))))

(defun origami-parser-sat (pred)
  (origami-do (pos <- (origami-parser-position))
              (a <- (origami-parser-item))
              (if (funcall pred a)
                  (origami-parser-return pos)
                (origami-parser-zero))))

(defun origami-parser-char (x)
  (origami-parser-sat (lambda (c) (equal x c))))

(defun origami-parser-conj (p1 p2)
  (lambda (content)
    (or (origami-run-parser p1 content)
        (origami-run-parser p2 content))))

(defun origami-parser-0+ (p)
  (origami-parser-conj
   (origami-parser-1+ p)
   (origami-parser-return nil)))

(defun origami-parser-1+ (p)
  ;; recursive isn't going to cut it in elisp
  (lambda (content)
    (let ((res (origami-run-parser p content))
          (acc nil))
      (while res
        (setq acc (cons (car res) acc))
        (setq content (cdr res))
        (setq res (origami-run-parser p content)))
      (when acc
        (cons (reverse acc) content)))))

(defun origami-parser-1? (p)
  (origami-parser-conj p (origami-parser-return nil)))

(defun origami-parser-paired (start end children is-open data)
  (origami-do (begin <- start)
              (children <- (origami-parser-0+ children))
              (end <- end)
              (origami-parser-return (origami-fold-node begin end
                                                        (funcall is-open begin end)
                                                        children
                                                        (funcall data begin end)))))

(defun origami-parser-consume-while (pred)
  (origami-do (positions <- (origami-parser-1+ (origami-parser-sat pred)))
              (origami-parser-return (car (last positions)))))

;;; TODO: always need to parse to max nesting, even if some of it gets ignored

;;; interactive utils

;;; TODO: delete or make buffer local
(defvar origami-tree (origami-fold-top-level-node))

(defun origami-get-cached-tree (buffer)
  ;; TODO:
  (debug-msg "old tree: %s" origami-tree)
  origami-tree)

(defun origami-store-cached-tree (buffer tree)
  ;; TODO:
  (debug-msg "new tree: %s" tree)
  (setq origami-tree tree))

(defun origami-build-tree (buffer parser)
  (when parser
    (with-current-buffer buffer
      (let ((contents (buffer-string)))
        (-> parser
          (origami-run-parser (origami-content 0 contents))
          car
          origami-fold-top-level-node)))))

(defun origami-get-parser (buffer is-open data)
  ;; TODO: remove hardcoding!
  (origami-parser-0+ (origami-do
                      (origami-parser-consume-while (lambda (x)
                                                      (not (equal x "{"))))
                      (origami-parser-paired (origami-parser-char "{")
                                             (origami-parser-char "}")
                                             (origami-do
                                              (origami-parser-consume-while (lambda (x) (and (not (equal x "}"))
                                                                                             (not (equal x "{")))))
                                              (origami-parser-1?
                                               (origami-parser-paired (origami-parser-char "{")
                                                                      (origami-parser-char "}")
                                                                      (origami-parser-zero) is-open data)))
                                             is-open data))))

(defun origami-was-previously-open? (tree)
  (lambda (beg end)
    (-> (origami-fold-find-node-with-range tree beg end)
      -last-item
      origami-fold-open-p)))

(defun origami-previous-data (tree)
  (lambda (beg end)
    (-> (origami-fold-find-node-with-range tree beg end)
      -last-item
      origami-fold-data)))

(defun origami-get-fold-tree (buffer)
  "Facade. Build the tree if it hasn't already been built
otherwise fetch cached tree."
  ;; TODO: caching -- don't parse again if there have been no edits since last time
  (origami-build-tree buffer
                      (origami-get-parser buffer
                                          (origami-was-previously-open?
                                           (origami-get-cached-tree buffer))
                                          (origami-previous-data
                                           (origami-get-cached-tree buffer)))))

;;; dsl

 ;;; commands

;;; TODO: should ensure that minor mode is enabled?
;;; TODO: extract common pattern

(defun origami-open-node (buffer point)
  (interactive (list (current-buffer) (point)))
  (let ((tree (origami-get-fold-tree buffer)))
    (-when-let (path (origami-fold-find-node-containing tree point))
      (debug-msg "open path: %s" path)
      (origami-fold-diff tree (origami-store-cached-tree buffer
                                                         (origami-fold-open-set path t))
                         (origami-create-overlay-from-fold-tree-fn buffer)
                         (origami-delete-overlay-from-fold-tree-fn buffer)
                         (origami-change-overlay-from-fold-node-fn buffer)))))

(defun origami-close-node (buffer point)
  (interactive (list (current-buffer) (point)))
  (let ((tree (origami-get-fold-tree buffer)))
    (-when-let (path (origami-fold-find-node-containing tree point))
      (origami-fold-diff tree (origami-store-cached-tree buffer
                                                         (origami-fold-open-set path nil))
                         (origami-create-overlay-from-fold-tree-fn buffer)
                         (origami-delete-overlay-from-fold-tree-fn buffer)
                         (origami-change-overlay-from-fold-node-fn buffer)))))

(defun origami-toggle-node (buffer point))

(defun origami-reset (buffer)
  ;; TODO: provide this to the user in case we get screwed up, maybe
  ;; use this when disabling the minor mode?
  (interactive)
  (origami-remove-all-overlays buffer)
  ;; TODO: remove fold ds
  )

;;; minor mode

;;; origami.el ends here
