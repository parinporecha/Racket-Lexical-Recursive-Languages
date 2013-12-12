;;; =========================
;;; =====================================
;;; Evaluator for the ARITHMETIC language
;;; =====================================

;;; Semantic Values
;;; ===============

;;; expressible-value ::=  number | boolean


#lang racket

(require eopl/eopl)
(require "ast.rkt")
(require "semantic-domains.rkt")
(require "op.rkt")

(provide
  eval-ast
  number
  boolean
  expressible-value?
  apply-prim-op
  envt 
  envt?
  empty-en
  extended-en
  lookup
  list-index
  bind-id
  bind-val
  make-pair-exp1
  make-pair-exp2
  mymap
)


;;; eval-ast : ast? -> expressible-value?
;;; eval-ast :  throws error 
(define-datatype envt envt?
   [empty-en (val null?)]
   [extended-en (ids (list-of symbol?)) (dvals (list-of number?)) (inner envt? )])
(define (lookup e x)
   (cases  envt e
     [extended-en (ids dvals inner) (let([ j (list-index ids x)]) (if(equal? j -1)  (lookup inner x) (list-ref dvals j)))]
     [else "not found"]
     ))
(define (list-index l x)
  (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i))

(define (bind-id b) 
   (cases bind b 
     [make-bind (id dval) (p id ( empty-en '() ) )]))
(define (bind-val b) 
   (cases bind b 
     [make-bind (id dval) dval]))
(define (make-pair-exp1 b) 
   (cases pair_ast b 
     [make-pair_ast (exp1 exp2) exp1]))
(define (make-pair-exp2 b) 
   (cases pair_ast b 
     [make-pair_ast (exp1 exp2) exp2]))

(define ( mymap f l e)
       (cond
         [(not (list? l)) "3nd arg not list"]
         [(null? l) '()]
         [else (append (list (f (first l) e )) (mymap f  (rest l) e ))]
       ))
(define ( p a env )
    (cases ast a
      [number (datum) datum]
      [boolean (datum) datum]
      [id-ref-ast (sym)  sym ]
      [prim-app (op rands)
        (let ([args (mymap  eval-ast rands env )])
          (apply-prim-op op args))]
      [if-ast (condition texp fexp)
            (if(eval-ast condition env )
                (eval-ast texp env )
                (eval-ast fexp env )
            )]
      [cond-ast (ifargs elsearg)
                (cond 
                [(null? ifargs)  (eval-ast elsearg env )]  
                [(eval-ast (make-pair-exp1 (first ifargs)) env ) (eval-ast (make-pair-exp2 (first ifargs)) env)]
                [else ( eval-ast  (cond-ast  (rest ifargs) elsearg) env )]                 
                 )]
      [assume (binds body)
         (let ([symbols (map bind-id binds)]
              [values (map (lambda (b) (eval-ast (bind-val b) env )) binds) ])
              (let ([new-env (extended-en symbols values env)])(eval-ast body new-env)))          
      ]
      ))



(define ( eval-ast a env )
    (cases ast a
      [number (datum) datum]
      [boolean (datum) datum]
      [id-ref-ast (sym) (lookup env sym )]
      [prim-app (op rands)
        (let ([args (mymap  eval-ast rands env )])
          (apply-prim-op op args))]
      [if-ast (condition texp fexp)
            (if(eval-ast condition env )
                (eval-ast texp env )
                (eval-ast fexp env )
            )]
      [cond-ast (ifargs elsearg)
                (cond 
                [(null? ifargs)  (eval-ast elsearg env )]  
                [(eval-ast (make-pair-exp1 (first ifargs)) env ) (eval-ast (make-pair-exp2 (first ifargs)) env)]
                [else ( eval-ast  (cond-ast  (rest ifargs) elsearg) env )]                 
                 )]
      [assume (binds body)
         (let ([symbols (map bind-id binds)]
              [values (map (lambda (b) (eval-ast (bind-val b) env )) binds) ])
              (let ([new-env (extended-en symbols values env)])(eval-ast body new-env)))          
      ]
      ))


(define match-sig?
  (lambda (sig? val)
    (sig? val)))

;;; apply-prim-op : [op-symbol? (listof expressible-value?)] -> expressible-value?

;;; apply-prim-op : throws error when number or type of args
;;; do not match the signature of op identified by opsym.

(define apply-prim-op
  (lambda (opsym args)
    (let* ([op (op-find opsym)]
           [sig (op-sig op)]
           [args-sig (rest sig)])
      (cond
       [(and
         (= (length args-sig) (length args))
         (andmap match-sig? args-sig args))
        (apply (op-prim op)  args)]
       [#t (error 'apply-prim-op "incorrect number or type of arguments to ~a" opsym)]))))

      
;;; Testing
;;; =======

;;; Racket's unit testing framework
;;;(require rackunit)
;;;(envt? ( empty-en '() ))
;;;(let ([n5 (number 5)]
;;;      [n7 (number 7)]
;;;      [bt (boolean #t)])
;;; (check-equal? (eval-ast  n5 ( empty-en '() )) 5 "eval-ast: n5 test")( empty-en '() )
;;; (check-equal? (eval-ast  bt ( empty-en '() )) #t "eval-ast")
;;;(check-equal? (eval-ast (prim-app '+ (list ( number 5) (number 7)) ( empty-en '() )) 12 "eval-ast: prim-app-test")
;;; (check-exn exn? (lambda () (eval-ast (prim-app '+ (list n5 bt)) ( empty-en '() ))) "eval-ast: +:incorrect-arg-types")
;;; (check-exn exn? (lambda () (eval-ast (prim-app '/ (list n5 (number 0))) ( empty-en '() ))) "/:incorrect-arg-types")
;;;  )
;;;(eval-ast (if-ast (boolean #t) ( number 5) (number 7) ) ( empty-en '() ))
;;;(eval-ast (assume (list (make-bind  (id-ref-ast 't) (number 6) ))  (id-ref-ast 't) ) ( empty-en '() ))
