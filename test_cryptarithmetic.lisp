;;;
;;; Simple and naive cryptarithmetic solver, using the simple constraint propagation framework
;;;

;(defpackage :simple-CPS-cryptarithmetic-solver
;  (:use :common-lisp :simple-constraints-propagation))

;(in-package :simple-CPS-cryptarithmetic-solver)
(in-package :simple-constraints-propagation)

;;; Cryptarithmetic
;;;   A puzzle where some letters represent distinct digits from 0 to 9, and make the arithmetic hold
;;;   For example:
;;;         SEND               9567
;;;     +   MORE           +   1085
;;;     ---------          --------- 
;;;        MONEY              10652
;;;
;;;     has a solution where O = 0, M = 1, Y = 2, E = 5, N = 6, D = 7, R = 8, and S = 9

(def-problem *send-more-money*
  ;; SEND + MORE = MONEY
  (def-nodes (S E N D O R Y) :in '(0 1 2 3 4 5 6 7 8 9))
  (def-nodes (c1 c2 c3) :in '(0 1))
  (def-node M :in '(1 2 3 4 5 6 7 8 9))
  ;; constraints
  (linear-sum D E    :to (* 10 c1) Y)
  (linear-sum N R c1 :to (* 10 c2) E)
  (linear-sum E O c2 :to (* 10 c3) N)
  (linear-sum S M c3 :to (* 10 M) O)
  (all-different S E N D O R Y M))
;;;

