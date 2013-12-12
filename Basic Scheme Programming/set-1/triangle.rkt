#lang racket

;;; triangle :: number, number -> number

(define triangle
   (lambda(side height)
      (* 0.5 side height)))

(provide triangle)