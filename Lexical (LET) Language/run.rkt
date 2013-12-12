#lang racket

;;; ============================================
;;; Top level driver for the ARITHMETIC language
;;; ============================================

;;; run
;;; ===
;;; run : any -> expressible-value?
;;; run : throws parse error if input does not parse.
;;; run : throws apply-prim-op error if prim operator is
;;;       not given the right # or types of arguments.

(provide
  run
)

(require "parser.rkt")
(require "eval-ast.rkt")

(define run
  (lambda (e)
    (eval-ast (parse e) ( empty-en '()))))


;;; Unit testing
;;; ============
;;;(require rackunit)

;;;(check-equal? 5 (run 5) "run 5")
;;;(check-equal? #t (run #t) "run #t")
;;;(check-equal? 7 (run '(+ 3 4)) "run (+ 3 4)")

;;;(check-equal? 14 (run '(* (+ 3 4) 2)) "run (* (+ 3 4) 2)")
;;;(check-exn  exn?
;;;            (lambda () (run '(+ *)))
;;;            "parse-error: (+ *)")

;;;(check-exn  exn?
;;;            (lambda () (run '(0? #t)))
;;;           "apply-prim-app-error: (0? #t)")


;;;(test-case "run"
;;;           (check-equal? (run '(my-if #t 3 4)) 3)
;;;           (check-equal? (run '(my-if (- 5 5) (+ (+ 3 4) 5) (my-if #t 3 5))) 12)
;;;           (check-equal? (run '(my-cond [#t 10 ] [(+ 1 1) #t] [else 5] )) 10)
;;;           (check-equal? (run '(my-cond [ (my-if #f -10 0) 101] [(+ 1 (- 1 2)) 10] [else 100])) 101)
;;;           (check-equal? (run '(assume [(x 10) (y 5)] (+ x y))) 15)
 ;;;          (check-equal? (run '(assume [(x (+ 10 (+ 5 9))) (y 0) (z (* 5  5)) ] (+ x (* z (+ y 10))))) 274))
