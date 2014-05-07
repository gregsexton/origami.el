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

;;; overlay manipulation

(defun origami-create-overlay (beg end buffer)
  (when (> (- end beg) 0)
    ;; TODO: adding 1 won't work for anything other than parsing at
    ;; the char level -- see TODO further down too
    (make-overlay (+ beg 1) end buffer)))

(defun origami-hide-node-overlay (node)
  (-when-let (ov (origami-fold-data node))
    ;; TODO: make all of this customizable
    (overlay-put ov 'invisible 'origami)
    (overlay-put ov 'display "...")
    (overlay-put ov 'face 'font-lock-comment-delimiter-face)))

(defun origami-show-node-overlay (node)
  (-when-let (ov (origami-fold-data node))
    (overlay-put ov 'invisible nil)
    (overlay-put ov 'display nil)
    (overlay-put ov 'face nil)))

(defun origami-hide-overlay-from-fold-tree-fn (node)
  (origami-fold-postorder-each node 'origami-hide-node-overlay))

(defun origami-show-overlay-from-fold-tree-fn (node)
  (origami-fold-postorder-each node 'origami-show-node-overlay))

(defun origami-change-overlay-from-fold-node-fn (old new)
  (if (origami-fold-open-p new)
      (origami-show-node-overlay old)
    (origami-hide-node-overlay new)))

(defun origami-remove-all-overlays (buffer)
  (with-current-buffer buffer
    (remove-overlays (point-min) (point-max) 'invisible 'origami)))

;;; fold structure

(defun origami-fold-node-raw (beg end open &optional children data)
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

(defun origami-fold-root-node (&optional children)
  ;; TODO: fix min and max
  (origami-fold-node-raw 1 10000 t children 'root))

(defun origami-fold-is-root-node? (node) (eq (origami-fold-data node) 'root))

(defun origami-fold-beg (node)
  (when node
    (if (origami-fold-is-root-node? node)
        (aref node 0)
      ;; TODO: decrementing to counter offset
      (- (overlay-start (origami-fold-data node)) 1))))

(defun origami-fold-end (node)
  (when node
    (if (origami-fold-is-root-node? node)
        (aref node 1)
      (overlay-end (origami-fold-data node)))))

(defun origami-fold-open-p (node) (when node (aref node 2)))

(defun origami-fold-open-set (node value)
  (when node
    (if (origami-fold-is-root-node? node)
        node
      (origami-fold-node-raw (origami-fold-beg node)
                             (origami-fold-end node)
                             value
                             (origami-fold-children node)
                             (origami-fold-data node)))))

(defun origami-fold-children (node) (when node (aref node 3)))

(defun origami-fold-children-set (node children)
  (when node
    (origami-fold-node-raw (origami-fold-beg node)
                           (origami-fold-end node)
                           (origami-fold-open-p node)
                           children
                           (origami-fold-data node))))

(defun origami-fold-data (node) (when node (aref node 4)))

(defun origami-fold-range-equal (a b)
  (and (equal (origami-fold-beg a) (origami-fold-beg b))
       (equal (origami-fold-end a) (origami-fold-end b))))

(defun origami-fold-state-equal (a b)
  (equal (origami-fold-open-p a) (origami-fold-open-p b)))

(defun origami-fold-replace-child (node old new)
  (origami-fold-children-set node
                             (cons new (remove old (origami-fold-children node)))))

(defun origami-fold-assoc (path f)
  "Rewrite the tree, replacing the node referenced by PATH with
F applied to the leaf."
  (cdr
   (-reduce-r-from (lambda (node acc)
                     (destructuring-bind (old-node . new-node) acc
                       (cons node (origami-fold-replace-child node old-node new-node))))
                   (let ((leaf (-last-item path))) (cons leaf (funcall f leaf)))
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

(defun origami-fold-map (f tree)
  "Map F over the tree. Replacing each node with the result of (f
node). The children cannot be manipulated using f as the map will
replace them. This cannot change the structure of the tree, just
the state of each node."
  (origami-fold-children-set
   (funcall f tree)
   (-map (lambda (node) (origami-fold-map f node))
         (origami-fold-children tree))))

(defun origami-fold-path-map (f path)
  "Map F over the nodes in path. As with `origami-fold-map',
children cannot be manipulated."
  (cond ((null path) nil)
        ((cdr path) (funcall f (origami-fold-replace-child (car path)
                                                           (cadr path)
                                                           (origami-fold-path-map f (cdr path)))))
        (t (funcall f (car path)))))

(defun origami-fold-find-deepest (tree pred)
  (when tree
    (when (funcall pred tree)
      (-if-let (child (-first pred (origami-fold-children tree)))
          (cons tree (origami-fold-find-deepest child pred))
        (list tree)))))

(defun origami-fold-find-path-with-range (tree beg end)
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

(defun origami-fold-find-path-containing (tree point)
  "Return the path to the most specific (deepest) node that
contains point, or null."
  (origami-fold-find-deepest tree
                             (lambda (node)
                               (and (<= (origami-fold-beg node) point)
                                    (>= (origami-fold-end node) point)))))

;;; TODO: why does this own copying data over? should it own copying over open status?
;;; TODO: not happy with this signature. Breaks abstraction layering.
(defun origami-fold-node (beg end open buffer &optional children previous-tree)
  ;; TODO: beg and end are superfluous
  ;; TODO: previous-tree is always true and this isn't guaranteed to produce an overlay
  (let ((overlay (or (-> (origami-fold-find-path-with-range previous-tree beg end)
                       -last-item
                       origami-fold-data)
                     (origami-create-overlay beg end buffer))))
    (origami-fold-node-raw beg end open children overlay)))

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

(defun origami-parser-put (x)
  (lambda (s) (cons nil x)))

(defun origami-parser-get-string ()
  (origami-do (content <- (origami-parser-get))
              (origami-parser-return (origami-content-string content))))

(defun origami-parser-drop (n)
  (origami-do (content <- (origami-parser-get))
              (origami-parser-put (origami-content-from content n))
              ;; TODO: substring will error if n is too large, guard against this
              (origami-parser-return (substring (origami-content-string content) 0 n))))

(defun origami-parser-take (n)
  (lambda (content)
    (let ((content-str (origami-content-string content)))
      (unless (s-blank? content-str)
        (let* ((len (length content-str))
               (n (if (> n len) len n)))
          (cons (substring content-str 0 n) (origami-content-from content n)))))))

(defun origami-parser-item ()
  (origami-parser-take 1))

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

(defun origami-parser-string (str)
  ;; take rather than recursion due to elisp
  (origami-do (prefix <- (origami-parser-take (length str)))
              (pos <- (origami-parser-position))
              (if (equal str prefix)
                  (origami-parser-return pos)
                (origami-parser-zero))))

(defun origami-parser-regex (rx)
  "Match the regex somewhere in the remaining string. Note you
have to prefix with '^' if you wish to match the beginning."
  (origami-do (str <- (origami-parser-get-string))
              (if (string-match rx str)
                  (origami-parser-drop (match-end 0))
                (origami-parser-zero))
              (origami-parser-position)))

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

(defun origami-parser-not (parser)
  (lambda (content)
    (if (origami-run-parser parser content)
        nil
      (origami-run-parser (origami-parser-item) content))))

(defun origami-parser-consume-while (parser)
  ;; TODO: this should really be 0+ but for some reason goes in to an infinte loop
  (origami-do (positions <- (origami-parser-1+ parser))
              (origami-parser-return nil)))

(defun origami-parser-paired (start end children create)
  (origami-do (begin <- start)
              (children <- (origami-parser-0+ children))
              (end <- end)
              (origami-parser-return (funcall create begin end children))))

;;; TODO: always need to parse to max nesting, even if some of it gets ignored

;;; interactive utils

;;; TODO: delete or make buffer local
(defvar origami-tree (origami-fold-root-node))

(defun origami-get-cached-tree (buffer)
  origami-tree)

(defun origami-store-cached-tree (buffer tree)
  ;; TODO:
  (debug-msg "new:")
  (debug-msg (setq origami-tree tree)))

(defun origami-was-previously-open? (tree beg end)
  (-if-let (node (-last-item (origami-fold-find-path-with-range tree beg end)))
      (origami-fold-open-p node)
    t))

(defun origami-build-tree (buffer parser)
  (when parser
    (with-current-buffer buffer
      (let ((contents (buffer-string)))
        (-> parser
          (origami-run-parser (origami-content 0 contents))
          car
          origami-fold-root-node)))))

(defun origami-get-parser (buffer)
  ;; TODO: remove hardcoding!
  (let ((create (lambda (beg end children)
                  (origami-fold-node beg end
                                     (origami-was-previously-open? (origami-get-cached-tree buffer) beg end)
                                     buffer
                                     children
                                     (origami-get-cached-tree buffer)))))
    (origami-parser-0+ (origami-do
                        (origami-parser-paired (origami-parser-regex "public .*{")
                                               (origami-parser-char "}")
                                               (origami-parser-consume-while
                                                (origami-parser-not (origami-parser-char "}")))
                                               create)))))

(defun origami-get-fold-tree (buffer)
  "Facade. Build the tree if it hasn't already been built
otherwise fetch cached tree."
  ;; TODO: caching -- don't parse again if there have been no edits since last time
  (debug-msg "old:")
  (debug-msg
   (origami-build-tree buffer (origami-get-parser buffer))))

;;; commands

;;; TODO: should ensure that minor mode is enabled?
;;; TODO: extract common pattern
;;; TODO: document

(defun origami-open-node (buffer point)
  (interactive (list (current-buffer) (point)))
  (let ((tree (origami-get-fold-tree buffer)))
    (-when-let (path (origami-fold-find-path-containing tree point))
      (origami-fold-diff tree (origami-store-cached-tree buffer
                                                         (origami-fold-assoc
                                                          path (lambda (node)
                                                                 (origami-fold-open-set node t))))
                         'origami-hide-overlay-from-fold-tree-fn
                         'origami-show-overlay-from-fold-tree-fn
                         'origami-change-overlay-from-fold-node-fn))))

(defun origami-show-node (buffer point)
  "Like `origami-open-node' but opens parent nodes recursively so
as to ensure seeing where POINT is."
  (interactive (list (current-buffer) (point)))
  (let ((tree (origami-get-fold-tree buffer)))
    (-when-let (path (origami-fold-find-path-containing tree point))
      (origami-fold-diff tree (origami-store-cached-tree buffer
                                                         (origami-fold-path-map
                                                          (lambda (node)
                                                            (origami-fold-open-set node t))
                                                          path))
                         'origami-hide-overlay-from-fold-tree-fn
                         'origami-show-overlay-from-fold-tree-fn
                         'origami-change-overlay-from-fold-node-fn))))

(defun origami-close-node (buffer point)
  (interactive (list (current-buffer) (point)))
  (let ((tree (origami-get-fold-tree buffer)))
    (-when-let (path (origami-fold-find-path-containing tree point))
      (origami-fold-diff tree (origami-store-cached-tree buffer
                                                         (origami-fold-assoc
                                                          path (lambda (node)
                                                                 (origami-fold-open-set node nil))))
                         'origami-hide-overlay-from-fold-tree-fn
                         'origami-show-overlay-from-fold-tree-fn
                         'origami-change-overlay-from-fold-node-fn))))

(defun origami-toggle-node (buffer point)
  (interactive (list (current-buffer) (point)))
  (let ((tree (origami-get-fold-tree buffer)))
    (-when-let (path (origami-fold-find-path-containing tree point))
      (origami-fold-diff tree (origami-store-cached-tree buffer
                                                         (origami-fold-assoc
                                                          path (lambda (node)
                                                                 (origami-fold-open-set
                                                                  node (not (origami-fold-open-p
                                                                             (-last-item path)))))))
                         'origami-hide-overlay-from-fold-tree-fn
                         'origami-show-overlay-from-fold-tree-fn
                         'origami-change-overlay-from-fold-node-fn))))

(defun origami-open-all-nodes (buffer)
  (interactive (list (current-buffer)))
  (let ((tree (origami-get-fold-tree buffer)))
    (origami-fold-diff tree (origami-store-cached-tree buffer
                                                       (origami-fold-map
                                                        (lambda (node)
                                                          (origami-fold-open-set node t))
                                                        tree))
                       'origami-hide-overlay-from-fold-tree-fn
                       'origami-show-overlay-from-fold-tree-fn
                       'origami-change-overlay-from-fold-node-fn)))

(defun origami-close-all-nodes (buffer)
  (interactive (list (current-buffer)))
  (let ((tree (origami-get-fold-tree buffer)))
    (origami-fold-diff tree (origami-store-cached-tree buffer
                                                       (origami-fold-map
                                                        (lambda (node)
                                                          (origami-fold-open-set node nil))
                                                        tree))
                       'origami-hide-overlay-from-fold-tree-fn
                       'origami-show-overlay-from-fold-tree-fn
                       'origami-change-overlay-from-fold-node-fn)))

(defun origami-show-only-node (buffer point)
  (interactive (list (current-buffer) (point)))
  (origami-close-all-nodes buffer)
  (origami-show-node buffer point))

(defun origami-reset (buffer)
  (interactive (list (current-buffer)))
  (origami-store-cached-tree buffer (origami-fold-root-node))
  (origami-remove-all-overlays buffer))

;;; minor mode

;;; origami.el ends here
