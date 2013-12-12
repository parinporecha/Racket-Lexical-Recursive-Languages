#lang racket

;;; volume-cylinder :: number, number -> number

(define volume-cylinder 
   (lambda(radius height)
      (* radius radius height 3.14)))

(provide volume-cylinder)