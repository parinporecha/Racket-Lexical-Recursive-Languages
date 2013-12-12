#lang racket

(define (profit t_price)
  (- (revenue t_price)
     (cost t_price)))

(define (revenue t_price)
  (*  (attendees t_price) t_price))

(define (cost t_price)
  (+ 180 
     (* .04 (attendees t_price))))


(define (attendees t_price)
  (if(> t_price 5.8)
   	0 
     ( + 120 ( * ( / ( - 5 t_price ) 0.10 ) 15 ) ) ) )


(provide attendees cost revenue profit)


;; When ticket price is $5.00, number of attendees = 120
;; So when price is $4.00, attendees = 270
;; and at $3.00, attendees = 420

;; When ticket price is $3.00, per show cost = $196.80
;; So when ticket price is $4.00, per show cost = $190.80
;; and at $5.00, per show cost = $184.80

;; When ticket price is $3.00, per show movie owner makes = $1260
;; So, when ticket price is $4.00, per show movie owner makes = $1080
;; and at $5.00, per show movie owner makes = $600

;; Thus, the best price is $3.00



