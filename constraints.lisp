;;;
;;; Simple constraint propagation framework to allow solving simple discrete puzzles such as sudoku
;;;

(defpackage :simple-constraints-propagation
  (:use :common-lisp))

(in-package :simple-constraints-propagation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities relating to sets, other operations such as intersection and union are provided by Common Lisp
(defun empty? (s) (null s))
(defun empty-set () nil)
(defun singleton? (s) (and s (null (cdr s))))
(defun singleton-of (v) (list v))
(defun set-size (s) (length s))
(defun singleton-in (x s)
  ;; singleton x in set s
  (member (car x) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main objects: node, constraint, problem
;;; Node represent variables whose values are being sought, and constraints gives the relationship about the variables.
;;; Problem simply group the variables and constraints them together.
;;; In solving a problem, the real set of possible values of the node is contained elsewhere.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(defstruct problem
  name
  nodes        ;; list of nodes
  constraints  ;; list of constraints
  ;;
  cur-id       ;; for giving integer id to nodes, as they are added to the problem
  node-names   ;; hash table mapping node name to node structure, node names are compared with equal
  )

(defvar *cur-problem* nil
  "The current problem, used in creating problem and solving them.")

(defun new-problem (name)
  (make-problem :name name :nodes nil :constraints nil
		:cur-id 0 :node-names (make-hash-table :test 'equal)))

(defun node-number (&optional (p *cur-problem*))
  (length (problem-nodes p)))
(defun node-by (name)
  (gethash name (problem-node-names *cur-problem*)))
(defun next-node-id ()
  (prog1 (problem-cur-id *cur-problem*)
    (incf (problem-cur-id *cur-problem*))))
(defun add-node (n)
  (push n (problem-nodes *cur-problem*))
  (setf (gethash (node-name n) (problem-node-names *cur-problem*)) n))
(defun add-constraint (c)
  (push c (problem-constraints *cur-problem*)))

(defmacro with-new-problem (name &body body)
  `(let ((*cur-problem* (new-problem ',name)))
     ,@body
     ;; properly register the constraints to their nodes
     (dolist (c (problem-constraints *cur-problem*))
       (constraint-reg-to-nodes c))
     *cur-problem*))
(defmacro def-problem (name &body body)
  ;; here name should be a symbol used to define special variable
  `(defparameter ,name (with-new-problem ,name ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(defstruct node
  id            ;; a 0-based number to identify the node in a particular problem
  name          ;; can be any object, e.g. a symbol, a number, a list (arr 10). Equality of name is compared with equal
  choices       ;; all the possibilities, i.e. the domain of the variable. The values are compared with eql.
  constraints   ;; the list of constraints that this node is involved in
  )

(defun new-node (name &key in)
  (add-node (make-node :id (next-node-id) :name name :choices in :constraints nil)))

(defmacro def-node (name &key in)
  `(new-node ',name :in ,in))
(defmacro def-nodes (names &key in)
  (let ((in-choices (gensym)))
    `(let ((,in-choices ,in))
       ,@(mapcar #'(lambda (name)
		     `(def-node ,name :in ,in-choices))
		 names))))

(defun node-add-constraint (n c)
  (push c (node-constraints n)))
(defun update-node (n)
  ;; the possible values of node n have been updated, need to check its related constraints to propagate the changes
  (dolist (c (node-constraints n) t)
    (if (null (propagate-constraint c n))
	(return-from update-node nil))))

(defvar *nodes-instance* nil
  "Will be set to a vector in solving a problem. Each slot in the vector corresponds to the set of possible values of a node at the time.")
(defun node-value (n)
  ;; the value is in fact a set, which we simply use a list for now
  (aref *nodes-instance* (node-id n)))
(defmacro node-value-of (n)
  `(aref *nodes-instance* (node-id ,n)))
(defun node-reduce-choice (n choices-to-remove)
  ;; choices-to-remove is also a set
  ;; If after removing the choice, there is no choice (i.e. contradiction), returns nil.
  ;; If the set of choices is changed, trigger constraint propagation.
  ;; Returns the newest possible choices after constraint propagation.
  (let* ((old-v (node-value n))
	 (new-v (setf (node-value-of n) (set-difference old-v choices-to-remove))))
    (unless (equal old-v new-v)
      (update-node n))
    ;; the values may have been changed during constraint propagation
    (node-value n)))
(defun node-set-value (n choice)
  ;; choice is not a set
  (setf (node-value-of n) (singleton-of choice))
  (update-node n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; constraint is an abstract class, there should be concrete subclasses for different types of constraints
(defclass constraint ()
  ((nodes :accessor nodes :initarg :nodes :initform nil)))

(defgeneric propagate-constraint (c n)
  (:documentation "For the constraint c, propagate the changes of node n to other involved nodes, if possible. If constraint is violated, returns nil."))
(defgeneric constraint-satisfied (c)
  (:documentation "Whether the constraint is satisfied."))

(defun constraint-reg-to-nodes (c)
  ;; the nodes of the constraint c are known, need to register this constraint in the nodes
  ;; Should be called by constraint constructor after a constraint is initialized.
  (dolist (n (nodes c))
    (node-add-constraint n c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all-different constraint
(defclass all-different-constraint (constraint)
  ())

(defmethod propagate-constraint ((c all-different-constraint) n)
  (if (singleton? (node-value n))
      (dolist (neighbor (nodes c) t)
	(unless (equal neighbor n)
	  ;; each time get the newest choices of node n, because it might have been updated
	  (if (null (node-reduce-choice neighbor (node-value n)))
	      (return-from propagate-constraint nil))))
      t))
(defmethod constraint-satisfied ((c all-different-constraint))
  ;; all nodes with single values should be distinct
  (let ((r (empty-set)))
    (dolist (n (nodes c) t)
      (let ((nv (node-value n)))
	(cond ((empty? nv) (return-from constraint-satisfied nil))
	      ((singleton? nv)
	       (if (singleton-in nv r)
		   (return-from constraint-satisfied nil)
		   (setf r (union nv r)))))))))

(defun new-all-different (ns)
  ;; ns is a list of nodes
  (add-constraint (make-instance 'all-different-constraint :nodes ns)))
(defun new-all-different-by-name (names)
  (new-all-different (mapcar #'node-by names)))

(defmacro all-different (&rest names)
  `(new-all-different-by-name ',names))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Solver using simple depth first search
(defun initial-order-to-try (&optional (ns (problem-nodes *cur-problem*)))
  (sort (mapcar #'(lambda (n) (cons n (set-size (node-value n))))
		ns)
	#'<
	:key #'cdr))

(defun valid-config ()
  ;; returns 'complete if valid and all nodes have a single value
  ;; returns 'valid if valid but not all nodes have known value
  ;; returns nil if invalid
  (dolist (c (problem-constraints *cur-problem*)
	   (if (every #'(lambda (n)
			  (singleton? (node-value n)))
		      (problem-nodes *cur-problem*))
	       'complete
	       'valid))
    (if (not (constraint-satisfied c))
	(return-from valid-config nil))))
(defun copy-config (&optional (config *nodes-instance*))
  (let ((v (make-array (length config))))
    (dotimes (i (length config) v)
      (setf (aref v i) (aref config i)))))

(defun try-node-at (n v nodes-to-try)
  ;; make a copy of the values config, then set node n to be singleton of v
  (let* ((*nodes-instance* (copy-config))
	 (ok (node-set-value n v)))
    (if ok (try-nodes *nodes-instance* nodes-to-try) nil)))

(defmacro get-next-node-to-try (ns)
  ;; only get singleton
  (let ((pos (gensym))
	(tmp (gensym)))
    `(let ((,pos (position-if-not #'(lambda (x) (singleton? (node-value (car x)))) ,ns)))
       (if ,pos
	   (let ((,tmp (nthcdr ,pos ,ns)))
	     (setf ,ns (cdr ,tmp))
	     (car ,tmp))
	   nil))))
(defun try-nodes (*nodes-instance* nodes-to-try)
  ;; nodes-to-try is in the form as returned by initial-order-to-try, i.e. list of (node . size)
  (if nodes-to-try
      (let ((state (valid-config)))
	(cond ((null state) nil)
	      ((eq state 'complete) *nodes-instance*)
	      (t ;; valid but not complete
	       (let ((n (get-next-node-to-try nodes-to-try)))
		 (if (null n)
		     (return-from try-nodes nil)
		     (dolist (v (node-value (car n)) nil)
		       (let ((res (try-node-at (car n) v nodes-to-try)))
			 (if res (return-from try-nodes res)))))))))
      nil))

(defun solve-DFS (p &key evidence evidence-by-name)
  ;; evidence is a list of (node . possibilities)
  ;; evidence-by-name is a list of (node-name . possibilities)
  ;; Returns a vector of singletons for the nodes if OK.
  ;; Returns nil if constraints cannot be satisfied.
  (let ((*cur-problem* p)
	(*nodes-instance* (make-array (node-number p) :initial-element nil)))
    ;; initialize the possibilities of nodes
    (dolist (n (problem-nodes *cur-problem*))
      (setf (node-value-of n) (node-choices n)))
    ;; add in the initial evidence
    (dolist (e evidence)
      (let ((n (car e)))
	(setf (node-value-of n) (intersection (node-value n) (cdr e)))))
    (dolist (e evidence-by-name)
      (let ((n (node-by (car e))))
	(setf (node-value-of n) (intersection (node-value n) (cdr e)))))
    ;; initial propagation of constraints, simply update every node
    (dolist (n (problem-nodes *cur-problem*))
      (update-node n))
    ;; start DFS, using an initial order of nodes to try
    (try-nodes *nodes-instance* (initial-order-to-try))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;