#lang racket

;;; area-pipe :: number, number, number -> number

(define area-pipe
   (lambda(inner_radius length thickness)
      (* 2 3.14 (+ length thickness) (+ thickness (* 2 inner_radius)))))

(provide area-pipe)