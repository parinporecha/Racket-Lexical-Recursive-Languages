;;; ==========================================================
;;; Abstract Syntax Specification of a language for arithmetic
;;; ==========================================================
;;; ast ::= num-ast | bool-ast | prim-app-ast


#lang racket

;; (require "env.rkt")
(require "semantic-domains.rkt")
(require eopl/eopl)

(provide
  ast
  ast?
  number
  if-ast
  id-ref-ast
  function
  app
;  dyn-function-call
  assume
  boolean
  *op-symbols*
  op-symbol?
  bind?
  conds?
  env
  env?
  empty-env
  extended-env
  bind
  bind?
  make-bind
  extended-rec-env
  recursive
  make-fbind
  fbind
  fbind?
  proc
  proc?
  make-op
  closure)

(define-datatype proc proc?
  [make-op
   (prim procedure?)
   (sig (list-of procedure?))]
  [closure
   (formals (list-of symbol?))
   (body ast?)
   (e env?)])

(define-datatype fbind fbind?
  [make-fbind (fb-id symbol?)
              (fb-formals (list-of symbol?))
              (fb-body ast?)])

(define-datatype bind bind?
  [make-bind (id symbol?) (ast ast?)])

              
(define-datatype env env? 
  [empty-env ]
  [extended-env (ids (list-of symbol?)) (dvals (list-of any/c)) (outer-env env?)]
  [extended-rec-env (fids (list-of symbol?)) (formals (list-of (list-of symbol?))) (bodies (list-of ast?)) (outer-env env?)])


(define *op-symbols*
  '(+ - * / =
    < <= eq? 0? > >= not))

;;; op-symbol? : symbol? -> bool
(define op-symbol?
  (lambda (x)
    (if (memq x *op-symbols*)
      #t
      #f)))

(define if-stmt? 
 (lambda (thing)
  (eq? thing 'if)))

(define parse-binds?
  (lambda (thing)
    (and 
     (list? thing)
     (eq? (length thing) 2)
     (symbol? (first thing))
     (ast? (list-ref thing 1)))))
              

(define conds?
  (lambda (thing)
    (and 
     (list? thing)
     (eq? (length thing) 2)
     (ast? (first thing))
     (ast? (list-ref thing 1)))))

(define-datatype ast ast?
  [number (datum number?)]
  [boolean (datum boolean?)]
  [id-ref-ast (datum symbol?)]
  [if-ast (condition ast?)
          (then-exp ast?)
          (else-exp ast?)]
  [assume (bindings (listof bind?))
          (eval-exp ast?)]
  [function (ids (listof symbol?))
            (body ast?)]
  [app (function-closure ast?)
                 (parameters (listof ast?))]
  [recursive (fbinds (list-of fbind?)) (body ast?)]
;  [dyn-function-call (function-closure ast?)
;                     (parameters (listof ast?))]
)


;;; unit Testing
;;; ============

;;; Racket's unit testing framework
(require rackunit)

;;; You should see nothing untoward when you load ast.ss
;;; using a require or load.  Try changing the ast? to
;;; number? if you want to see an error reported by the
;;; rackunit unit testing framework

