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
(defun print-crypt-term (term)
  (dolist (x term) (format t "~A" x))
  (format t " = ")
  (dolist (x term) (format t "~A" (let ((v (node-value (node-by x))))
				    (if (singleton? v)
					(singleton-value v)
					#\?))))
  (terpri))

(defun solve-cryptarithmetic (terms sum)
  ;; terms is a list of list of symbols, such as ((S E N D) (M O R E)), which represents the terms to add
  ;; sum is a list of symbols, such as (M O N E Y), which represents the sum of the terms
  ;; Assumes the leading digits are non-zero, and assume the sum is as least as long as each term
  ;; Assume there are only a small number of terms.
  (let* ((nonzero-digits (remove-duplicates (cons (first sum) (mapcar #'first terms))))
	 (all-digits (remove-duplicates (apply #'append sum terms)))
	 (free-digits (set-difference all-digits nonzero-digits))
	 (max-carry (length terms)) ;; conservative estimate
	 (carry-digits (loop for i from 0 to max-carry collect i))
	 (cols (do ((r nil (cons (mapcar #'first ts) r))
		    (ts (mapcar #'reverse terms) (mapcar #'cdr ts)))
		   ((every #'null ts) (nreverse r))))
	 (first-carry (gensym "carry"))
	 (carrys (mapcar #'(lambda (x) (declare (ignore x)) (gensym "carry")) cols))
	 (p
	  (with-new-problem 'cryptarithmetic
	    ;; nodes
	    (dolist (n nonzero-digits) (new-node n :in '(1 2 3 4 5 6 7 8 9)))
	    (dolist (n free-digits) (new-node n :in '(0 1 2 3 4 5 6 7 8 9)))
	    (new-node first-carry :in '(0))
	    (dolist (c carrys) (new-node c :in carry-digits))
	    ;; constraints
	    (new-all-different-by-name all-digits)
	    (do ((ts cols (cdr ts))
		 (cs carrys (cdr cs))
		 (p-cs (cons first-carry carrys) (cdr p-cs))
		 (ss (reverse sum) (cdr ss)))
		((null ts)
		 (when ss
		   (new-linear-sum-by-name (list (car ss) (car p-cs)) :coefficients '(-1 1))
		   (dolist (n (cdr ss)) (new-linear-sum-by-name n :coefficients '(-1)))))
	      (let* ((col (remove nil (car ts)))
		     (ns `(,(car ss) ,(car cs) ,(car p-cs) ,@col))
		     (coefs `(-1 -10 1 ,@(mapcar #'(lambda (x) (declare (ignore x)) 1) col))))
		(new-linear-sum-by-name ns :coefficients coefs))))))
    ;;
    (solve-dfs p :print-func #'(lambda (x)
				 (declare (ignore x))
				 (dolist (n all-digits)
				   (format t "Node ~A = ~A~%" n (node-value (node-by n))))
				 (terpri)
				 (dolist (term terms)
				   (print-crypt-term term))
				 (print-crypt-term sum)))))

;;; http://bach.istc.kobe-u.ac.jp/llp/crypt.html
;;
;  SEND =  9567
;  MORE =  1085
; MONEY = 10652
(solve-cryptarithmetic '((S E N D) (M O R E)) '(M O N E Y))
;; 
;  SATURN =  546790
;  URANUS =  794075
; PLANETS = 1340865
(solve-cryptarithmetic '((S A T U R N) (U R A N U S)) '(P L A N E T S))
;;
; KYOTO = 41373
; OSAKA = 32040
; TOKYO = 73413
(solve-cryptarithmetic '((K Y O T O) (O S A K A)) '(T O K Y O))
;;
;  BLACK =  79208
;  GREEN =  53446
; ORANGE = 132654
(solve-cryptarithmetic '((B L A C K) (G R E E N)) '(O R A N G E))
;;
;  CRASH =  36845
; HACKER = 583926
; REBOOT = 620771
(solve-cryptarithmetic '((C R A S H) (H A C K E R)) '(R E B O O T))
;;
;  APPLE =  63374
;  GRAPE =  90634
; CHERRY = 154008
(solve-cryptarithmetic '((A P P L E) (G R A P E)) '(C H E R R Y))
;;
;  BANJO =  93784
;  VIOLA =  10423
; VIOLIN = 104207
(solve-cryptarithmetic '((B A N J O) (V I O L A)) '(V I O L I N))
;;
;  HAIKU =  92315
;  SUSHI =  45493
; KIMONO = 137808
(solve-cryptarithmetic '((H A I K U) (S U S H I)) '(K I M O N O))
;;
;  BASIC =  60852
;  LOGIC =  47352
; PASCAL = 108204
(solve-cryptarithmetic '((B A S I C) (L O G I C)) '(P A S C A L))
;;
;  POTATO =  168486
;  TOMATO =  863486
; PUMPKIN = 1031972
(solve-cryptarithmetic '((P O T A T O) (T O M A T O)) '(P U M P K I N))
;;
;    SIX =    650
;  SEVEN =  68782
;  SEVEN =  68782
; TWENTY = 138214
(solve-cryptarithmetic '((S I X) (S E V E N) (S E V E N)) '(T W E N T Y))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

