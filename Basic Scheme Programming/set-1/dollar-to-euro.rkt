#lang racket

;;; dollar->euro :: number -> number

(define dollar->euro
   (lambda(x)
      (* x 0.76)))

(provide dollar->euro)