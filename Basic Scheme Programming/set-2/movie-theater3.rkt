#lang racket

(define movie_cost 180)
(define ticket_cost 0.04)
(define people 120)
(define more_attend 15)
(define price_less 0.10)
(define per_t_price 5.00)


(define (profit t_price)
  (- (revenue t_price)
     (cost t_price)))

(define (revenue t_price)
  (*  (attendees t_price) t_price))

(define (cost t_price)
  (+ movie_cost 
     (* ticket_cost (attendees t_price))))

(define (attendees t_price)
  (+ people
     (* (/ more_attend price_less) (- per_t_price t_price))))


(provide profit revenue cost attendees)
