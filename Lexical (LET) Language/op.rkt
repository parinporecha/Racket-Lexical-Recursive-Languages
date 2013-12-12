#lang racket


(provide
  *ops*
  op-name
  op-prim
  op-sig
  op-find)
  
(require eopl/eopl)
(require "ast.rkt")
(require "semantic-domains.rkt")


;;; nonzero? : any/c -> boolean?
(define nonzero?
  (lambda (n)
    (and (number? n)
      (not (zero? n)))))

;;; Operators
;;; =========
(define-struct op (name prim sig))
(define +op    (make-op '+  +   (list number? number? number?)))
(define -op    (make-op '-     -     (list number? number? number?)))
(define *op    (make-op '*     *     (list number? number? number?)))
(define /op    (make-op '/     /     (list number? number? nonzero?)))
(define <op    (make-op '<     <     (list boolean? number? number?)))
(define <=op   (make-op '<=    <=    (list boolean? number? number?)))
(define >op    (make-op '>     >     (list boolean? number? number?)))
(define >=op   (make-op '>=    >=    (list boolean? number? number?)))
(define =op    (make-op '=   =   (list boolean? expressible-value? expressible-value?)))
(define !op    (make-op '!    not (list boolean? number?)))

(define *ops*
  (list +op -op *op /op <op <=op  >op  >=op =op  !op ))

(define op-find
  (lambda (opsym)
    (findf (lambda (op)
             (eq? opsym (op-name op)))
           *ops*)))