;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname syntax-expander) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require eopl/eopl)

(require "ast.rkt")
(require "parser.rkt")

(provide syntax-expander
         if-ast
         prim-app
         number
         boolean)

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