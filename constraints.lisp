;;;
;;; Simple constraint propagation framework to allow solving simple discrete puzzles such as sudoku
;;;

(defpackage :simple-constraints-propagation
  (:use :common-lisp))

(in-package :simple-constraints-propagation)


;;; utilities relating to sets, other operations such as intersection and union are provided by Common Lisp
(defun empty? (s) (null s))
(defun singleton? (s) (and s (null (cdr s))))

;;; main objects: node, constraint
;;; Node represent variables whose values are being sought, and constraints gives the relationship about the variables.
;;; In solving a problem, the real set of possible values of the node is contained elsewhere.
(defstruct node
  id            ;; a 0-based number to identify the node in a particular problem
  name          ;; can be any object, e.g. a symbol, a number, a list (arr 10). Equality of name is compared with equal
  possibilities ;; all the possibilities, i.e. the domain of the variable
  constraints   ;; the list of constraints that this node is involved in
  )

(defun def-node (name possibilities)
  ;; the id need to be set properly for a problem before solving it.
  (make-node :id 0 :name name :possibilities possibilities :constraints nil))
(defun node-add-constraint (n c)
  (push c (node-constraints n)))
(defun update-node (n)
  ;; the possible values of node n have been updated, need to check its related constraints to propagate the changes
  (dolist (c (constraints n))
    (if (null (propagate-constraint c n))
	(return-from update-node nil))))

(defvar *nodes-instance* nil
  "Will be set to a vector in solving a problem. Each slot in the vector corresponds to the set of possible values of a node at the time.")
(defun node-value (n)
  ;; the value is in fact a set, which we simply use a list for now
  (aref *nodes-instance* (node-id n)))
(defmacro node-value-of (n)
  `(aref *nodes-instance* (node-id ,n)))
(defun node-reduce-choice (n choice-to-remove)
  ;; choice-to-remove is also a set
  ;; If after removing the choice, there is no choice (i.e. contradiction), returns nil.
  ;; If the set of choices is changed, trigger constraint propagation.
  ;; Returns the newest possible choices after constraint propagation.
  (let* ((old-v (node-value n))
	 (new-v (setf (node-value-of n) (set-difference old-v choice-to-remove))))
    (unless (equal old-v new-v)
      (udpate-node n))
    ;; the values may have been changed during constraint propagation
    (node-value n)))
;;;;
;;; constraint is an abstract class, there should be concrete subclasses for different types of constraints
(defclass constraint ()
  ((nodes :accessor nodes :initarg nodes :initform nil)))

(defgeneric propagate-constraint (c n)
  (:documentation "For the constraint c, propagate the changes of node n to other involved nodes, if possible. If constraint is violated, returns nil."))

(defun constraint-reg-to-nodes (c)
  ;; the nodes of the constraint c are known, need to register this constraint in the nodes
  ;; Should be called by constraint constructor after a constraint is initialized.
  (dolist (n (nodes c))
    (node-add-constraint n c)))

;; Permutation constraint
(defclass permutation-constraint (constraint)
  ((possibilities :accessor possibilities :initarg possibilities :initform nil)))

(defmethod propagate-constraint ((c permutation-constraint) n)
  (dolist (neighbor (nodes c))
    (unless (equal neighbor n)
      ;; each time get the newest choices of node n, because it might have been updated
      (if (null (node-reduce-choice neighbor (node-value n)))
	  (return-from propagate-constraint nil))))
  t)

(defun def-permutation (choices nodes)
  (let ((c (make-instance 'permutation-constraint :nodes nodes :possibilities choices)))
    (constraint-reg-to-nodes c)
    c))
;;;
