#lang racket


(provide
  expressible-value?
)

;;; expressible-value? is the set of things that are the
;;; results of evaluation of expressions (asts).

;;; expressible-value? : any/c -> boolean?
(define expressible-value?
  (lambda (thing)
    (or (number? thing)
      (boolean? thing))))
