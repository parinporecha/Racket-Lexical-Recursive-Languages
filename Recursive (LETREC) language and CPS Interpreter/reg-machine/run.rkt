#lang racket

(provide
  run
  *init-env*)
(require "ast.rkt")
(require "parser.rkt")
(require "eval-ast.rkt")
(require "env-rec.rkt")
(require "op.rkt")

(define *init-env* (extended-env '(+ - * / < > <= = >= not 0?) *ops* (empty-env)))

(define run
  (lambda (e)
    (eval-ast/k (parse e) *init-env* *top-k*)))

