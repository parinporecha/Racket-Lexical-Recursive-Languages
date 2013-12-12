;;; ===============================================
;;; Operator Handler for the LEXICAL (LET) language
;;; ===============================================

#lang racket


(provide
  *ops*)
  
(require eopl/eopl)
(require "env-rec.rkt")
(require "semantic-domains.rkt")
(require "ast.rkt")


;;; nonzero? : any/c -> boolean?
(define nonzero?
  (lambda (n)
    (and (number? n)
      (not (zero? n)))))

;;; Operators
;;; =========
(define +op    (make-op  +   (list number? number? number?)))
(define -op    (make-op     -     (list number? number? number?)))
(define *op    (make-op     *     (list number? number? number?)))
(define /op    (make-op     /     (list number? number? nonzero?)))
(define <op    (make-op     <     (list boolean? number? number?)))
(define >op    (make-op     >     (list boolean? number? number?)))
(define <=op   (make-op    <=    (list boolean? number? number?)))
(define =op    (make-op  eq?   (list boolean? expressible-value? expressible-value?)))
(define >=op   (make-op    >=    (list boolean? number? number?)))
(define notop    (make-op  not   (list boolean? boolean?)))
(define 0?    (make-op  zero?   (list boolean? number?)))

(define *ops*
  (list +op -op *op /op <op >op <=op =op >=op notop 0?))
