;;;
;;; Simple and naive sudoku solver, using the simple constraint propagation framework
;;;

;;(defpackage :simple-CPS-sudoku-solver
;;  (:use :common-lisp :simple-constraints-propagation))

;; should manually load "constraints.lisp" first

;;(in-package :simple-CPS-sudoku-solver)
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
;;; more irregular shaped sudoku
(defun list-to-regions (ns)
  ;; ns is a list of number that give region number to each cell in the 9x9 board, in row by row order.
  ;; returns a list of list of cell names that belong to the same group.
  (let ((h (make-hash-table))
        (r nil))
    (dotimes (i 9)
      (dotimes (j 9)
        (let ((n (pop ns)))
          (push (list 'cell i j) (gethash n h)))))
    ;;
    (maphash #'(lambda (k v) (declare (ignore k)) (push v r)) h)
    r))

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
                 (pr-cell (singleton-num (node-value (node-by (list 'cell i j)))))
                 (if (or (= j 2) (= j 5)) (format t "|")))
               (format t "|~%")))
      ;;
      (dotimes (i 9)
        (pr-sep (if (= 0 (mod i 3)) sep2 sep1))
        (pr-row i))
      (pr-sep sep2))))

(defun solve-sudoku-list (ns)
  (solve-DFS *classic-sudoku* :evidence-by-name (list-to-evidence ns) :print-func #'print-board))

(defun solve-irregular-sudoku-list (ns gs)
  ;; ns gives the initial config, gs gives the regions
  (let ((p (with-new-problem irregular-sudoku
             ;; 9x9 nodes, row by row
             (dotimes (i 9)
               (dotimes (j 9)
                 (new-node (list 'cell i j) :in '(1 2 3 4 5 6 7 8 9))))
             ;; constraints
             ;; row
             (dotimes (i 9)
               (new-all-different-by-name (loop for j from 0 to 8 collect (list 'cell i j))))
             ;; column
             (dotimes (j 9)
               (new-all-different-by-name (loop for i from 0 to 8 collect (list 'cell i j))))
             ;; cells in each region should be distinct
             (dolist (g (list-to-regions gs))
               (new-all-different-by-name g)))))
    (solve-DFS p :evidence-by-name (list-to-evidence ns) :print-func #'print-board)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defparameter *test-very-hard*
  '(0 0 0  0 0 4  1 0 6
    0 2 0  9 0 1  0 0 0
    0 0 0  0 6 0  7 0 0

    0 7 0  0 8 6  2 0 0
    0 0 2  0 0 0  5 0 0
    0 0 5  3 9 0  0 1 0

    0 0 3  0 2 0  0 0 0
    0 0 0  8 0 9  0 3 0
    8 0 9  4 0 0  0 0 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; squiggly-sudoku testing instances
;; easy
;; http://www.dailysudoku.com/sudoku/archive.shtml?year=2013&month=06&day=21&type=squiggly
(defparameter *test-squiggly-easy*
  '(0 0 2  0 0 0  0 0 3
    5 0 6  0 0 0  8 9 0
    0 0 9  3 0 0  0 4 6

    0 0 0  4 0 3  0 0 2
    2 0 0  5 8 7  0 0 9
    1 0 0  6 0 8  0 0 0

    4 8 0  0 0 6  2 0 0
    0 2 1  0 0 0  6 0 8
    6 0 0  0 0 0  3 0 0))
(defparameter *test-squiggly-g-easy*
  '(0 0 0  0 1 1  1 2 2
    0 0 0  0 1 1  1 2 2
    3 3 0  1 1 4  2 2 2

    3 3 3  3 1 4  4 2 2
    3 3 3  4 4 4  5 5 5
    6 6 4  4 7 5  5 5 5

    6 6 6  4 7 7  8 5 5
    6 6 7  7 7 8  8 8 8
    6 6 7  7 7 8  8 8 8))
;; medium
;; http://www.dailysudoku.com/sudoku/archive.shtml?year=2013&month=06&day=6&type=squiggly
(defparameter *test-squiggly-medium*
  '(0 0 0  0 0 3  0 0 1
    0 0 0  0 0 9  0 0 0
    0 6 5  0 0 0  0 0 0

    0 1 3  0 0 2  0 0 0
    5 4 0  8 0 1  0 3 9
    0 0 0  5 0 0  7 1 0

    0 0 0  0 0 0  5 4 0
    0 0 0  3 0 0  0 0 0
    7 0 0  9 0 0  0 0 0))
(defparameter *test-squiggly-g-medium*
  '(0 0 1  1 1 1  1 2 2
    0 0 0  1 1 1  2 2 2
    7 0 0  0 1 2  2 2 3

    7 7 0  8 8 8  2 3 3
    7 7 7  8 8 8  3 3 3
    7 7 6  8 8 8  4 3 3

    7 6 6  6 5 4  4 4 3
    6 6 6  5 5 5  4 4 4
    6 6 5  5 5 5  5 4 4))
;; hard
;; http://www.dailysudoku.com/sudoku/archive.shtml?year=2013&month=06&day=5&type=squiggly
(defparameter *test-squiggly-hard*
  '(4 0 0  0 1 0  3 0 0
    0 0 0  0 0 0  0 9 0
    0 0 8  0 0 0  6 0 5

    0 8 0  0 0 9  0 0 0
    3 9 0  8 0 6  0 4 2
    0 0 0  5 0 0  0 8 0

    9 0 4  0 0 0  1 0 0
    0 2 0  0 0 0  0 0 0
    0 0 6  0 5 0  0 0 9))
(defparameter *test-squiggly-g-hard*
  '(0 0 0  0 1 1  1 2 2
    0 0 0  1 1 1  2 2 2
    3 0 0  1 1 1  2 2 2

    3 3 3  4 4 4  2 5 5
    3 3 3  4 4 4  5 5 5
    3 3 6  4 4 4  5 5 5

    6 6 6  7 7 7  8 8 5
    6 6 6  7 7 7  8 8 8
    6 6 7  7 7 8  8 8 8))
;; very hard
;; http://www.dailysudoku.com/sudoku/archive.shtml?year=2013&month=06&day=22&type=squiggly
(defparameter *test-squiggly-very-hard*
  '(0 9 1  0 0 8  0 0 0
    0 0 0  0 7 0  0 0 0
    0 0 0  4 0 0  5 0 9

    0 0 0  0 0 0  0 1 0
    6 7 0  0 0 0  0 4 1
    0 4 0  0 0 0  0 0 0

    3 0 7  0 0 5  0 0 0
    0 0 0  0 4 0  0 0 0
    0 0 0  1 0 0  6 8 0))
(defparameter *test-squiggly-g-very-hard*
  '(0 1 1  1 2 2  2 2 2
    0 0 1  1 1 2  2 2 5
    0 0 0  1 1 1  2 5 5

    0 0 3  4 4 4  5 5 5
    0 3 3  4 4 4  5 5 8
    3 3 3  4 4 4  5 8 8

    3 3 6  7 7 7  8 8 8
    3 6 6  6 7 7  7 8 8
    6 6 6  6 6 7  7 7 8))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main functions to use are solve-sudoku-list and solve-irregular-sudoku-list
;; e.g. for regular ones
;; (solve-sudoku-list *test-easy*)
;; (solve-sudoku-list *test-medium*)
;; (solve-sudoku-list *test-hard*)
;; (solve-sudoku-list *test-evil*)
;; (solve-sudoku-list *test-very-hard*)

;; e.g. for irregular ones
;; (solve-irregular-sudoku-list *test-squiggly-easy* *test-squiggly-g-easy*)
;; (solve-irregular-sudoku-list *test-squiggly-medium* *test-squiggly-g-medium*)
;; (solve-irregular-sudoku-list *test-squiggly-hard* *test-squiggly-g-hard*)
;; (solve-irregular-sudoku-list *test-squiggly-very-hard* *test-squiggly-g-very-hard*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
