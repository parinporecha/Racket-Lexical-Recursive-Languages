#lang racket

;;; sum-coins :: number, number, number, number -> number

(define sum-coins
   (lambda(pennies nickels dimes quarters)
      (+ (* pennies 0.01) (* nickels 0.05) (* dimes 0.1) (* quarters 0.25))))

(provide sum-coins)