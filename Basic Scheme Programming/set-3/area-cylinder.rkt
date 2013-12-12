#lang racket

;;; area-cylinder :: number, number -> number

(define area-cylinder
   (lambda(radius height)
      (* 2 3.14 radius (+ radius height))))

(provide area-cylinder)