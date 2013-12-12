#lang racket

;;; ===================================
;;; Parser for the ARITHIMETIC language
;;; ===================================

;;; concrete syntax
;;; ===============
;;; exp ::= <number> | <boolean> | (<op> exp ...)
;;; op  ::= one of *op-symbols*

(require eopl/eopl)
(require "ast.rkt")
(require "semantic-domains.rkt")
;(require "syntax-expander.rkt")

(provide
  parse
  make-bindings)

;;; syntax-expander :: Expand a cond statment into nested ifs
;;; syntax-expander :: any/c -> ast?
(define syntax-expander
  (lambda (d)
    (let ([if-cond (list-ref d 1)])
      (if (eq? (first if-cond) 'else)
          (parse (list-ref if-cond 1))
          (if-ast (parse (first if-cond))
                  (parse (list-ref if-cond 1))
                  (parse (cons 'cond (get-sublist d 2 (length d)))))))))

;;; get-sublist :: gives a sublist between two indices
;;; get-sublist :: [list? number? number?] -> list?
(define get-sublist
  (lambda (l start end)
    (letrec ([get-sub-items 
              (lambda (l s e index)
                (cond
                  [(>= index (length l)) '()]
                  [(> index e) '()]
                  [(< index s) (get-sub-items l s e (+ index 1))]
                  [else (cons (list-ref l index) (get-sub-items l s e (+ index 1)))]))])
      (get-sub-items l start end 0))))



;;; parse
;;; =====
;;; parse : any/c -> ast?
;;; parse : throws parse-error when input
;;;         does not parse.

(define parse
  (lambda (d)
    (cond
      [(number? d) (number d)]
      [(boolean? d) (boolean d)]
      [(and 
          (list? d) 
          (eq? (length d) 4)
          (eq? (first d) 'if-ast))
       (if-ast (parse (list-ref d 1)) (parse (list-ref d 2)) (parse (list-ref d 3)))] 
      [(and 
        (list? d) 
        (eq? (first d) 'cond))
       (syntax-expander d)]
      [(and (list? d)
            (eq? (first d) 'assume)
            (eq? (length d) 3))
       (assume (make-bindings (first (rest d))) (parse (second (rest d))))]
      [(and (list? d)
            (eq? (first d) 'proc)
            (eq? (length d) 3))
       (function (list-ref d 1)
                 (parse (list-ref d 2)))]
      [(and (list? d)
            (eq? (first d) '@)
            (eq? (length d) 3))
       (app (parse (list-ref d 1)) (map parse (list-ref d 2)))]
      [(and (list? d)
            (eq? (first d) 'recursive)
            (eq? (length d) 3))
       (recursive (map (lambda(x) (make-fbind (first x) (second x) (parse (third x)))) (second d)) (parse (third d)))]
;      [(and (list? d)
;            (eq? (first d) '$)
;            (eq? (length d) 3))
;       (dyn-function-call (parse (list-ref d 1)) (map parse (list-ref d 2)))]
      [(symbol? d) (id-ref-ast d)]
      [(and (list? d)
            (not (null? d)))
       (app (parse (first d)) (map parse (rest d)))]
      [else (parse-error d)]
)))

;;; make-bindings
;;; =====
;;; make-bindings : (list-of list?) -> (list-of bind?)
;;; make-bindings : throws parse-error when input
;;;         does not parse. 
(define make-bindings 
  (lambda(l)
    (if
    (and
    (list? l)
    (not (null? l)))
    (map make-binds l)
    (parse-error l))))

;;; make-binds
;;; =====
;;; make-binds : list? -> bind?
;;; make-binds : throws parse-error when input
;;;         does not parse. 
(define make-binds
  (lambda(l)
    (if
    (and
     (list? l)
     (eq? (length l) 2)
     (symbol? (first l)))
    (make-bind (first l) (parse (first (rest l))))
    (parse-error l))))
;;; .


;;; parse-error
;;; ===========
;;; parse-error : any/c -> void?
(define parse-error
  (lambda (d)
    (error 'parse-error "invalid syntax ~a" d)))
;;; .


;;; Unit testing
;;; ============
;(require rackunit)

;(check-equal? (number 5)   (parse 5) "parse5")

;(check-equal? (boolean #t) (parse #t) "parse#t")

;(check-equal? (prim-app '+ (list (number 3)))
;              (parse '(+ 3)) "parse+")

;(check-equal? (prim-app '/ (list (number 3) (boolean #t)))
;              (parse '(/ 3 #t)) "parse/")

;(check-exn exn? (lambda () (parse 'x)) "parse-error-x")

;(check-exn exn? (lambda () (parse '())) "parse-error-nil")

;(parse '(assume [(a 1) (b -10) (x 4)] (assume [(a 10) (b 2) (c -3)] (assume [(a 5)] (+ a b x)))))


;(parse '(cond [(if (<= 1 2) #t #f) (if (= 1 1) (+ 3 2) (- 3 2))] [(if #t #t #f) #t]))
;(parse '(if (if (= (if (> (+ 10 1) (* 10 1)) 10 0) (* 5 2)) #f #t) 1 0))


