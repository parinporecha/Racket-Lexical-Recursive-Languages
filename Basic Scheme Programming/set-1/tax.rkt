#lang racket

;;; tax :: number -> number

(define tax
   (lambda(pay)
      (/ (* pay 15) 100)))

(provide tax)
