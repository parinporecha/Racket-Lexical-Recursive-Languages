#lang racket

(define (profit t_price)
  ( - (revenue t_price) (cost t_price)))


(define (revenue t_price)
  ( * (attendees t_price) t_price))


(define (cost t_price)
  ( * (attendees t_price) 1.5 ))



(define (attendees t_price)
  (+ 120
     (* (/ 15 .10) (- 5.00 t_price))))



(provide profit revenue cost attendees)
