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
(require 'cl)
(require 'parser)
(require 'origami-parsers)

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
  (if (origami-fold-open? new)
      (origami-show-node-overlay old)
    (origami-hide-node-overlay new)))

(defun origami-remove-all-overlays (buffer)
  (with-current-buffer buffer
    (remove-overlays (point-min) (point-max) 'invisible 'origami)))

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

(defun origami-fold-root-node (&optional children)
  ;; TODO: fix min and max
  (origami-fold-node 1 100000 t children 'root))

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

(defun origami-fold-open? (node) (when node (aref node 2)))

(defun origami-fold-open-set (node value)
  (when node
    (if (origami-fold-is-root-node? node)
        node
      (origami-fold-node (origami-fold-beg node)
                         (origami-fold-end node)
                         value
                         (origami-fold-children node)
                         (origami-fold-data node)))))

(defun origami-fold-children (node) (when node (aref node 3)))

(defun origami-fold-children-set (node children)
  (when node
    (origami-fold-node (origami-fold-beg node)
                       (origami-fold-end node)
                       (origami-fold-open? node)
                       children
                       (origami-fold-data node))))

(defun origami-fold-data (node) (when node (aref node 4)))

(defun origami-fold-range-equal (a b)
  (and (equal (origami-fold-beg a) (origami-fold-beg b))
       (equal (origami-fold-end a) (origami-fold-end b))))

(defun origami-fold-state-equal (a b)
  (equal (origami-fold-open? a) (origami-fold-open? b)))

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
  ;; TODO: this isn't recursive. Not sure that it should be, looking at
  ;; usage. Fix or rename.
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

(defun origami-fold-preorder-reduce (tree f initial-state)
  "Reduce the tree by doing a preorder traversal. F is applied
with the current state and the current node at each iteration."
  (-reduce-from (lambda (state node) (origami-fold-preorder-reduce node f state))
                (funcall f initial-state tree)
                (origami-fold-children tree)))

(defun origami-fold-postorder-reduce (tree f initial-state)
  "Reduce the tree by doing a postorder traversal. F is applied
with the current state and the current node at each iteration."
  (funcall f (-reduce-from (lambda (state node) (origami-fold-postorder-reduce node f state))
                           initial-state
                           (origami-fold-children tree))
           tree))

;;; interactive utils

;;; TODO: create functions for accessing/setting the local vars and
;;; make sure these are used -- error if the buffer local var isn't set

(defun origami-get-cached-tree (buffer)
  (or (local-variable-p 'origami-tree buffer)
      (error "Necessary local variables were not available"))
  (buffer-local-value 'origami-tree buffer))

(defun origami-store-cached-tree (buffer tree)
  (or (local-variable-p 'origami-tree buffer)
      (local-variable-p 'origami-tree-tick buffer)
      (error "Necessary local variables were not available"))
  (with-current-buffer buffer
    (setq origami-tree-tick (buffer-modified-tick))
    (setq origami-tree tree)))

(defun origami-rebuild-tree? (buffer)
  "Determines if the tree needs to be rebuilt for BUFFER since it
was last built."
  (not (= (buffer-local-value 'origami-tree-tick buffer)
          (buffer-modified-tick buffer))))

(defun origami-build-tree (buffer parser)
  (when parser
    (with-current-buffer buffer
      (let ((contents (buffer-string)))
        (-> parser
          (parser-run (parser-content 0 contents))
          car
          origami-fold-root-node)))))

(defun origami-get-parser (buffer)
  (let* ((cached-tree (origami-get-cached-tree buffer))
         (create (lambda (beg end children)
                   (let ((previous-fold (-last-item (origami-fold-find-path-with-range cached-tree beg end))))
                     (origami-fold-node beg end
                                        (if previous-fold (origami-fold-open? previous-fold) t)
                                        children
                                        (or (-> (origami-fold-find-path-with-range
                                                 (origami-get-cached-tree buffer) beg end)
                                              -last-item
                                              origami-fold-data)
                                            (origami-create-overlay beg end buffer)))))))
    (-when-let (parser-gen (cdr (assoc (buffer-local-value 'major-mode buffer)
                                       origami-parser-alist)))
      (funcall parser-gen create))))

(defun origami-get-fold-tree (buffer)
  "Facade. Build the tree if it hasn't already been built
otherwise fetch cached tree."
  (when origami-mode
    (if (origami-rebuild-tree? buffer)
        (origami-build-tree buffer (origami-get-parser buffer))
      (origami-get-cached-tree buffer))))

(defun origami-apply-new-tree (buffer old-tree new-tree)
  (when new-tree
    (origami-fold-diff old-tree (origami-store-cached-tree buffer new-tree)
                       'origami-hide-overlay-from-fold-tree-fn
                       'origami-show-overlay-from-fold-tree-fn
                       'origami-change-overlay-from-fold-node-fn)))

;;; commands

;;; TODO: document all commands

(defun origami-open-node (buffer point)
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-fold-find-path-containing tree point))
      (origami-apply-new-tree buffer tree (origami-fold-assoc path (lambda (node)
                                                                     (origami-fold-open-set node t)))))))

(defun origami-open-node-recursively (buffer point)
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-fold-find-path-containing tree point))
      (origami-apply-new-tree
       buffer tree (origami-fold-assoc path
                                       (lambda (node)
                                         (origami-fold-map (lambda (node)
                                                             (origami-fold-open-set node t))
                                                           node)))))))

(defun origami-show-node (buffer point)
  "Like `origami-open-node' but opens parent nodes recursively so
as to ensure seeing where POINT is."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-fold-find-path-containing tree point))
      (origami-apply-new-tree buffer tree (origami-fold-path-map
                                           (lambda (node)
                                             (origami-fold-open-set node t))
                                           path)))))

(defun origami-close-node (buffer point)
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-fold-find-path-containing tree point))
      (origami-apply-new-tree buffer tree (origami-fold-assoc
                                           path (lambda (node)
                                                  (origami-fold-open-set node nil)))))))

(defun origami-close-node-recursively (buffer point)
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-fold-find-path-containing tree point))
      (origami-apply-new-tree
       buffer tree (origami-fold-assoc path
                                       (lambda (node)
                                         (origami-fold-map (lambda (node)
                                                             (origami-fold-open-set node nil))
                                                           node)))))))

(defun origami-toggle-node (buffer point)
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-fold-find-path-containing tree point))
      (origami-apply-new-tree buffer tree (origami-fold-assoc
                                           path (lambda (node)
                                                  (origami-fold-open-set
                                                   node (not (origami-fold-open?
                                                              (-last-item path))))))))))

(defun origami-forward-toggle-node (buffer point)
  (interactive (list (current-buffer) (point)))
  (let (end)
    (with-current-buffer buffer
      (save-excursion
        (goto-char point)
        (setq end (line-end-position))))
    (-when-let (tree (origami-get-fold-tree buffer))
      (-when-let (path (origami-fold-find-path-containing tree point))
        (let ((forward-node (-first (lambda (node)
                                      (and (>= (origami-fold-beg node) point)
                                           (<= (origami-fold-beg node) end)))
                                    (origami-fold-children (-last-item path)))))
          (let ((path (if forward-node (append path (list forward-node)) path)))
            (origami-apply-new-tree buffer tree (origami-fold-assoc
                                                 path (lambda (node)
                                                        (origami-fold-open-set
                                                         node (not (origami-fold-open?
                                                                    (-last-item path)))))))))))))

(defun origami-open-all-nodes (buffer)
  (interactive (list (current-buffer)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (origami-apply-new-tree buffer tree (origami-fold-map
                                         (lambda (node)
                                           (origami-fold-open-set node t))
                                         tree))))

(defun origami-close-all-nodes (buffer)
  (interactive (list (current-buffer)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (origami-apply-new-tree buffer tree (origami-fold-map
                                         (lambda (node)
                                           (origami-fold-open-set node nil))
                                         tree))))

(defun origami-show-only-node (buffer point)
  (interactive (list (current-buffer) (point)))
  (origami-close-all-nodes buffer)
  (origami-show-node buffer point))

(defun origami-previous-fold (buffer point)
  "Move point to the beginning of the fold before point. If point
is in a fold, move to the beginning of the fold that point is
in."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-> tree
      (origami-fold-preorder-reduce (lambda (state n)
                                      (cons (origami-fold-beg n) state)) nil)
      (->> (-reduce (lambda (state pos)
                      (if (< state point) state pos))))
      goto-char)))

(defun origami-next-fold (buffer point)
  "Move point to the end of the fold after point. If point is in
a fold, move to the end of the fold that point is in."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-> tree
      (origami-fold-postorder-reduce (lambda (state n)
                                       (cons (origami-fold-end n) state)) nil)
      (->> (-reduce (lambda (state pos)
                      (if (<= pos point) state pos))))
      goto-char)))

(defun origami-reset (buffer)
  (interactive (list (current-buffer)))
  (origami-store-cached-tree buffer (origami-fold-root-node))
  (origami-remove-all-overlays buffer))

;;; minor mode

(defvar origami-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `origami-mode'.")

(defcustom origami-mode-hook nil
  "Hook called when origami minor mode is activated or deactivated."
  :type 'hook
  :group 'origami)

(define-minor-mode origami-mode
  "Minor mode to selectively hide/show text in the current buffer.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Lastly, the normal hook `origami-mode-hook' is run using
`run-hooks'.

Key bindings:
\\{origami-mode-map}"
  :group 'origami
  :lighter nil
  :keymap origami-mode-map
  :init-value nil
  (if origami-mode                      ;enabling if t
      (progn
        (set (make-local-variable 'origami-tree) (origami-fold-root-node))
        (set (make-local-variable 'origami-tree-tick) (buffer-modified-tick)))
    (origami-reset (current-buffer))))

(provide 'origami)

;;; origami.el ends here
