#lang racket
   
;;; Fahrenheit->Celsius :: number -> number
(provide/contract 
 [Fahrenheit->Celsius (-> number? number?)])


(define Fahrenheit->Celsius 
   (lambda(x)
      (* (- x 32) (/ 5 9))))
