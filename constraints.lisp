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
(defun singleton-value (s)
  ;; assume s is singleton
  (car s))

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
;;; linear-sum constraint
;;; a0*x0 + a1*x1 + ... + ak*xk = c
;;; where ai and c are constants (any Lisp number), and xi are node names (variables)
;;; The xi need not be distinct when creating the constraint (to allow more flexibility),
;;;   but internally only the distinct variables are kept, and their coefficients are added together.
;;;   Internally only variables with non-zero coefficients are kept.
(defclass linear-sum-constraint (constraint)
  ((coefficients :accessor coefficients :initarg :coefficients :initform nil)
   (constant :accessor constant :initarg :constant :initform 0) ;; in the same order as nodes
   ))

(defmethod propagate-constraint ((c linear-sum-constraint) n)
  ;; when all but one node has a single possibility, can determine the remaining one
  (cond ((find-if #'null (nodes c) :key #'node-value) nil)
	((singleton? (node-value n))
	 (let* ((unknown-n1 (position-if-not #'singleton? (nodes c) :key #'node-value))
		(unknown-n2 (if unknown-n1
				(position-if-not #'singleton? (nodes c) :key #'node-value :start (1+ unknown-n1))))
		(should-be ;; the value of the remaining node. nil if still not determined or no need to determine any
		 (if (or (null unknown-n1) unknown-n2) ;; either all known, or more than one unknown
		     nil
		     (do ((sum (constant c) (if (= i unknown-n1)
						sum
						(- sum (* (car cs) (singleton-value (node-value (car ns)))))))
			  (ns (nodes c) (cdr ns))
			  (cs (coefficients c) (cdr cs))
			  (i 0 (1+ i)))
			 ((or (null ns) (null cs))
			  (/ sum (nth unknown-n1 (coefficients c))))))))
	   (or (null should-be)
	       (let ((unknown-node (nth unknown-n1 (nodes c))))
		 (and (member should-be (node-value unknown-node))
		      (node-set-value unknown-node should-be))))))
	(t t)))

(defmethod constraint-satisfied ((c linear-sum-constraint))
  (do ((sum 0 (+ sum (* (car cs) (singleton-value (node-value (car ns))))))
       (ns (nodes c) (cdr ns))
       (cs (coefficients c) (cdr cs)))
      ((or (null ns) (null cs))
       (= sum (constant c)))
    (let ((nv (node-value (car ns))))
      (cond ((empty? nv) (return-from constraint-satisfied nil))
	    ((not (singleton? nv))
	     (return-from constraint-satisfied t))))))

(defun new-linear-sum-by-name (names &key (coefficients (mapcar #'(lambda (x) (declare (ignore x)) 1) names)) (constant 0))
  ;; names are list of node names
  ;; names may have duplicates, need to add their coefficients.
  ;; and zero (combined) coefficients will be discarded together with the variable
  (let ((h (make-hash-table :test 'equal))
	(ns nil)
	(coefs nil))
    (do ((vs names (cdr vs))
	 (cs coefficients (cdr cs)))
	((null vs))
      (incf (gethash (car vs) h 0) (if cs (car cs) 1)))
    ;;
    (maphash #'(lambda (k v)
		 (when (/= v 0)
		   (push k ns)
		   (push v coefs)))
	     h)
    ;;(format t "linear-sum:~% constant:~A~% coefs:~A~% names:~A~%~%" constant coefs ns)
    (add-constraint (make-instance 'linear-sum-constraint
				   :constant constant
				   :coefficients coefs
				   :nodes (mapcar #'node-by ns)))))

(defmacro linear-sum (&rest terms)
  ;; E.g. (linear-sum x0 (* k1 x1) :to x2 :consts 2 3) means x0 + k1*x1 = x2 + 2 + 3
  ;; The :to is optional. If not provided, all monomials are on the left hand side, but the constant is always on the right hand side
  ;; anything before :consts are a monomial, which describe ai*xi
  ;; anything after :consts should be constants and will be summed up, this is the expected sum
  ;; Each term before :consts and not :to can be one of
  ;;  1. (- node-name), equivalent to (* 1 node-name)
  ;;  2. (* k node-name), k is the coefficient
  ;;  3. node-name, where the node-name is not confused with the above two cases
  (flet ((normalize-terms (ts coef)
	   (mapcar #'(lambda (x)
			   (if (consp x)
			       (cond ((eq '- (car x)) `(* ,(- coef) ,(second x)))
				     ((eq '* (car x)) `(* ,(* coef (second x)) ,(third x)))
				     (t `(* ,coef ,x)))
			       `(* ,coef ,x)))
		   ts)))
    (let* ((p (position :consts terms))
	   (ts (subseq terms 0 p))
	   (p-to (position :to ts))
	   (LHS (normalize-terms (subseq ts 0 p-to) 1))
	   (RHS (normalize-terms (if p-to (subseq ts (1+ p-to))) -1))
	   (vars (append LHS RHS))
	   (consts (if p (subseq terms (1+ p)) nil)))
    `(new-linear-sum-by-name ',(mapcar #'third vars)
			     :coefficients ',(mapcar #'second vars)
			     :constant ,(if consts `(reduce #'+ ',consts :initial-value 0) 0)))))
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
(defun print-config (ns)
  ;; a simple default printing
  (format t "Node Values:~%")
  (dolist (n ns)
    (format t "Node ~A: ~A~%" (node-name n) (node-value n)))
  (terpri))

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

(defun solve-DFS (p &key evidence evidence-by-name (print-func #'print-config))
  ;; evidence is a list of (node . possibilities)
  ;; evidence-by-name is a list of (node-name . possibilities)
  ;; Returns a vector of singletons for the nodes if OK.
  ;; Returns nil if constraints cannot be satisfied.
  ;; print-func is a function of the list of nodes, and it should print the node values nicely
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
    ;; print the initial puzzle first
    (format t "Initial:~%")
    (funcall print-func (problem-nodes *cur-problem*)) ;; the function is expected to take node values using node-value
    (terpri)
    ;; initial propagation of constraints, simply update every node
    (dolist (n (problem-nodes *cur-problem*))
      (update-node n))
    ;; start DFS, using an initial order of nodes to try
    (let ((res (try-nodes *nodes-instance* (initial-order-to-try))))
      (cond (res (format t "After solving:~%")
		 (let ((*nodes-instance* res))
		   (funcall print-func (problem-nodes *cur-problem*))
		   (terpri))
		 res)
	    (t (format t "Cannot solve the puzzle.~%")
	       nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;