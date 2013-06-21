;;;
;;; Simple and naive sudoku solver, using the simple constraint propagation framework
;;;

;(defpackage :simple-CPS-sudoku-solver
;  (:use :common-lisp :simple-constraints-propagation))

;(in-package :simple-CPS-sudoku-solver)
(in-package :simple-constraints-propagation)

;;; Sudoku:
;;;   A board contains 3x3 large squares, each large square contains 3x3 small squares, so a board has totally 9x9 small squares
;;;   Initially some small squares are filled with 1 to 9, and the rest are empty.
;;;   The goal is to fill in the empty small squares with 1 to 9, such that
;;;     1. each row contains a permutation of 1, 2, ..., 9
;;;     2. each column contains a permutation of 1, 2, ..., 9
;;;     3. each large square contains a permutation of 1, 2, ..., 9

(defparameter *classic-sudoku-nodes* (make-array '(9 9) :initial-element nil))
(def-problem *classic-sudoku*
  ;; 9x9 nodes, row by row
  (dotimes (i 9)
    (dotimes (j 9)
      (setf (aref *classic-sudoku-nodes* i j) (new-node (list 'cell i j) :in '(1 2 3 4 5 6 7 8 9)))))
  ;; constraints
  ;; row
  (dotimes (i 9)
    (new-all-different (loop for j from 0 to 8 collect (aref *classic-sudoku-nodes* i j))))
  ;; column
  (dotimes (j 9)
    (new-all-different (loop for i from 0 to 8 collect (aref *classic-sudoku-nodes* i j))))
  ;; squares
  (dolist (sq-x '(0 3 6))
    (dolist (sq-y '(0 3 6))
      (let ((ns nil))
	(dotimes (i 3)
	  (dotimes (j 3)
	    (push (aref *classic-sudoku-nodes* (+ sq-x i) (+ sq-y j)) ns)))
	(new-all-different ns)))))

;;;
(defun list-to-evidence (ns)
  ;; ns is a list of initial squares of the puzzle, row by row
  ;; Turn it into the form of list of ((cell i j) initial-value), as evidence
  (let ((e nil))
    (dotimes (i 9)
      (dotimes (j 9)
	(let ((n (pop ns)))
	  (if (/= n 0)
	      (push `((cell ,i ,j) ,n) e)))))
    e))

(defun singleton-num (s)
  (if (singleton? s)
      (car s)
      nil))
(defun print-board (ns)
  (declare (ignore ns))
  (let ((sep1 "+---+---+---+")
	(sep2 "+===+===+===+"))
    (labels ((pr-sep (sp)
	       (dotimes (i 3) (format t "~a" sp))
	       (terpri))
	     (pr-cell (n) (format t "| ~a " (if n n #\space)))
	     (pr-row (i)
	       (dotimes (j 9)
		 (pr-cell (singleton-num (node-value (aref *classic-sudoku-nodes* i j))))
		 (if (or (= j 2) (= j 5)) (format t "|")))
	       (format t "|~%")))
      ;;
      (dotimes (i 9)
	(pr-sep (if (= 0 (mod i 3)) sep2 sep1))
	(pr-row i))
      (pr-sep sep2))))

(defun solve-sudoku-list (ns)
  (solve-DFS *classic-sudoku* :evidence-by-name (list-to-evidence ns) :print-func #'print-board))
;;; Test problems
;;
(defparameter *test-easy*
  '(0 0 0  3 0 5  0 6 0
    3 0 7  6 0 4  0 0 0
    0 8 0  0 0 9  3 0 4

    9 0 0  1 3 0  0 0 8
    0 6 4  0 2 0  9 7 0
    8 0 0  0 4 7  0 0 1

    2 0 5  8 0 0  0 9 0
    0 0 0  7 0 3  2 0 5
    0 9 0  4 0 2  0 0 0))
(defparameter *test-medium*
  '(0 0 0  0 0 0  1 3 8
    5 9 0  0 0 1  0 7 0
    3 0 0  0 0 4  0 0 0

    0 0 0  9 4 0  3 8 1
    4 0 0  0 1 0  0 0 7
    1 5 8  0 7 3  0 0 0

    0 0 0  6 0 0  0 0 9
    0 7 0  1 0 0  0 5 3
    8 1 3  0 0 0  0 0 0))
(defparameter *test-hard*
  '(0 7 0  0 8 0  0 0 2
    8 0 1  0 0 0  0 9 0
    0 2 0  0 9 5  0 0 1

    4 0 0  0 0 0  6 0 0
    0 0 0  2 1 9  0 0 0
    0 0 9  0 0 0  0 0 3

    1 0 0  5 6 0  0 2 0
    0 6 0  0 0 0  1 0 7
    5 0 0  0 4 0  0 3 0))
(defparameter *test-evil*
  '(0 8 0  0 0 5  0 0 0
    1 0 2  0 7 0  0 4 0
    0 0 3  0 0 0  6 0 0

    0 0 8  0 0 7  9 5 0
    0 0 0  0 1 0  0 0 0
    0 6 1  3 0 0  8 0 0

    0 0 4  0 0 0  1 0 0
    0 1 0  0 6 0  2 0 8
    0 0 0  5 0 0  0 9 0))
;;;
