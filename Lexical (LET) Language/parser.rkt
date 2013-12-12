#lang racket

;;; ===================================
;;; Parser for the ARITHIMETIC language
;;; ===================================

;;; concrete syntax
;;; ===============
;;; exp ::= <number> | <boolean> | <id> | (<op> exp ...) 
;;;         (if <exp> <exp> ...) | 
;;;         (cond [<exp> <exp>]..[else <exp>])
;;;         (assume [<id> <exp>] ... <exp>)         
;;; op  ::= one of *op-symbols*

(require "ast.rkt")

(provide  parse)

;;; parse
;;; =====
;;; parse : any/c -> ast?
;;; parse : throws parse-error when input
;;;         does not par
(define func 
    (lambda (d)
       
        (make-pair_ast (parse (first d))(parse (first (rest d))))))
(define func1 
    (lambda (d)
       
        (make-bind (parse (first d))(parse (first (rest d))))))
(define parse
  (lambda (d)
    (cond
      [(number? d) (number d)]
      [(boolean? d) (boolean d)]
      [(symbol? d) (id-ref-ast d)]
      [(and  (list? d) (not (null? d)) (memq (first d) *op-symbols*)) (prim-app (first d) (map parse (rest d)))]
      [(and  (list? d) (not (null? d)) (match (first d) ['if #t][else #f])  (equal? (length d) 4))
       (if-ast ( parse (first (rest d))) ( parse (first (rest (rest d)))) 
                       ( parse ( first (rest (rest (rest d))))))]
      [(and (list? d)(not (null? d))(match(first d)['cond #t][else #f]))
        (cond-ast (map func (reverse (rest (reverse (rest d )))))(parse (first (rest (last d))))) ]
      [(and (list? d)(not (null? d))(match(first d)['assume #t][else #f]))
       (assume (map  func1  (first (rest d))) (parse (first (rest (rest d)))))]
      [else (parse-error d)])))

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
;;;(require rackunit)

;;;(check-equal? (number 5)   (parse 5) "parse5")

;;;(check-equal? (boolean #t) (parse #t) "parse#t")

;;;(check-equal? (prim-app '+ (list (number 3)))
;;;              (parse '(+ 3)) "parse+")

;;;(check-equal? (prim-app '/ (list (number 3) (boolean #t)))
 ;;;             (parse '(/ 3 #t)) "parse/")



;;;(check-exn exn? (lambda () (parse '())) "parse-error-nil")



;;;(parse  5)  
;;;(parse '(if (  if #f a b) 3 7)) 
;;;(parse  '(+ 1 2))
;;;(parse '(if (+ 1 2) 3 7))
;;;(parse '(cond [#f 6] [#t 34][else 23]))
;;;(parse '(assume [(x (+ 10 (+ 5 9))) (y 0) (z (* 5  5)) ] (+ x (* z (+ y 10)))))

