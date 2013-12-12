#lang racket

;;; total-profit :: number -> number

(define total-profit
   (lambda(attendees)
      (- (* attendees 4.5) 20)))

(provide total-profit)