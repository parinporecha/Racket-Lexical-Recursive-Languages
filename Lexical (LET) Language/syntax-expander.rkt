#lang racket

;;; ===================================
;;; syntax-expander for the ARITHIMETIC language
;;; ===================================

;;; concrete syntax
;;; ===============
;;; exp ::= <number> | <boolean> | <id> | (<op> exp ...) 
;;;         (if <exp> <exp> ...) | 
;;;         (cond [<exp> <exp>]..[else <exp>])
;;;         (assume [<id> <exp>] ... <exp>)         
;;; op  ::= one of *op-symbols*
;;; ==========================================================
;;; Abstract Syntax Specification of a language for arithmetic
;;; ==========================================================
;;; ast ::= num-ast | bool-ast | id-ast |  prim-app-ast |
;;;    if-ast | cond-ast| assume-ast




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






(provide  syntax-expander)

;;; syntax-expander
;;; =====
;;; syntax-expander : any/c -> ast?
;;; syntax-expander : throws syntax-expander-error when input
;;;         does not par
(define func 
    (lambda (d)
        (make-pair_ast (syntax-expander (first d))(syntax-expander (first (rest d))))))

(define func11 
    (lambda (d)
         (syntax-expander (first d))))

(define func22 
    (lambda (d)
        (syntax-expander (first (rest d)))))

(define func1 
    (lambda (d)
        (make-bind (syntax-expander (first d))(syntax-expander (first (rest d))))))

(define syntax-expander
  (lambda (d)
    (cond
      [(number? d) (number d)]
      [(boolean? d) (boolean d)]
      [(symbol? d) (id-ref-ast d)]
      [(and  (list? d) (not (null? d))
             (memq (first d) *op-symbols*))
       (prim-app (first d) (map syntax-expander (rest d)))]
      [(and   (list? d) (not (null? d)) (match (first d) ['if #t][else #f])  (equal? (length d) 4))
       (if-ast ( syntax-expander (first (rest d))) ( syntax-expander (first (rest (rest d)))) 
                       ( syntax-expander ( first (rest (rest (rest d))))))
      ]
      [(and (list? d)(not (null? d))(match(first d)['cond #t][else #f]))
       (cond[(null? (rest (rest d)))  ( func22 (first (reverse d))) ]
            [else (if-ast ( func11 (first (rest d))) ( func22 (first (rest d)))
                  (syntax-expander (cons 'cond (rest (rest d))) ))                    
            ]                        
                           
            )
       ]                                 
      
      [(and (list? d)(not (null? d))(match(first d)['assume #t][else #f]))
       
       (assume (map  func1  (first (rest d))) (syntax-expander (first (rest (rest d)))))
       ]
           
      [else (syntax-expander-error d)])))

;;; .


;;; syntax-expander-error
;;; ===========
;;; syntax-expander-error : any/c -> void?
(define syntax-expander-error
  (lambda (d)
    (error 'syntax-expander-error "invalid syntax ~a" d)))
;;; .


;;; Unit testing
;;; ============
;;;(require rackunit)

;;;(check-equal? (number 5)   (syntax-expander 5) "syntax-expander5")

;;;(check-equal? (boolean #t) (syntax-expander #t) "syntax-expander#t")

;;;(check-equal? (prim-app '+ (list (number 3)))
;;;              (syntax-expander '(+ 3)) "syntax-expander+")

;;;(check-equal? (prim-app '/ (list (number 3) (boolean #t)))
;;;              (syntax-expander '(/ 3 #t)) "syntax-expander/")



;;;(check-exn exn? (lambda () (syntax-expander '())) "syntax-expander-error-nil")


;;;(syntax-expander '(cond [(if #f #f #t) 10 ] [(+ 3 4) 5] [else 10]))
;;;(syntax-expander '(cond [#f 6] [#t 34][else 23]))
;;;(syntax-expander '(cond [else 23]))


