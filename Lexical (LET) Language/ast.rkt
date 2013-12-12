;;; ==========================================================
;;; Abstract Syntax Specification of a language for arithmetic
;;; ==========================================================
;;; ast ::= num-ast | bool-ast | id-ast |  prim-app-ast |
;;;    if-ast | cond-ast| assume-ast


#lang racket

(require eopl/eopl)

(provide
  ast
  ast?
  number
  boolean
  id-ref-ast
  prim-app
  if-ast
  cond-ast
  assume
  bind 
  bind?
  make-bind
  *op-symbols*
  op-symbol?
  pair_ast 
  pair_ast?
  make-pair_ast
)

(define *op-symbols*
  '(+ - * / <= < = > >= ! ))

;;; op-symbol? : symbol? -> bool
(define op-symbol?
  (lambda (x)
    (if (memq x *op-symbols*) #t #f)))

(define-datatype bind bind?
   [make-bind (id ast?) (dval ast?)])
(define-datatype pair_ast pair_ast?
   [make-pair_ast (exp1 ast?) (exp2 ast?)])

(define-datatype ast ast?
  [number (datum number?)]
  [boolean (datum boolean?)]
  [id-ref-ast (sym symbol?)]
  [prim-app (op op-symbol?) (rands (list-of ast?))]
  [if-ast (condition ast?) (texp ast?) (fexp ast?)]
  [cond-ast (ifargs (list-of pair_ast?)) (elsearg ast?)]
  [assume (binds (list-of bind?)) (body ast?)]
)
;;; unit Testing
;;; ============

;;;(ast? (number 4))
;;;(ast? (boolean #t))
;;;(ast? ( id-ref-ast 'u))
;;;(ast? (prim-app '+ (list ( id-ref-ast 'u) ( id-ref-ast 'u))))
;;;(ast? (if-ast ( prim-app '> (list (number 4) (number 5))) (number 4) (number 5)))
;;;(ast? (cond-ast (list (make-pair_ast (number 4) (number 7) ))  (number 5)))
;;;(ast? (assume (list (make-bind  (id-ref-ast 'u) (number 6) ))  (number 5) ) )
