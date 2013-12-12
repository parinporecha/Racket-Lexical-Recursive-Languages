#lang racket

(require eopl/eopl)
(require "ast.rkt")
(require "env-rec.rkt")
(require "semantic-domains.rkt")
(require "op.rkt")

(provide
  eval-ast/k
  *top-k*)

(define *top-k*
 (lambda(thing)
  thing))

(define apply-k
  (lambda(k v)
    (k v)))

(define eval-ast/k
  (lambda (a e k)
    (cases ast a
      [number (x) (apply-k k x)]
      [id-ref-ast (sym) (lookup-env/k e sym k (lambda() (format "unbound identifier ~a" sym)))]
      [boolean (x) (apply-k k x)]
      [if-ast (condition then else) (eval-ast/k condition e (lambda(x) (eval-ast/k (if x then else) e k)))]
      [app (rator rands)
            (eval-ast/k rator e 
	      (lambda(vp)
	       (map/k (lambda(x next) (eval-ast/k x e next)) rands (lambda(args) (apply-proc/k vp args k)))))]
      
      [assume (binds body)
      	      (map/k get-bind-id/k binds
	       (lambda(ids)
		(map/k
		 (lambda(x n) (bind-ast/k x (lambda(ast) (eval-ast/k ast e n))))
		 binds
		 (lambda(vals)
		  (let ([newe (extended-env ids vals e)])
		   (eval-ast/k body newe k))))))]
      [function (formals body) (apply-k k (closure formals body e))]
      [recursive (fbinds body)
      		 (map/k get-fbind-fid/k fbinds
		  (lambda(fids)
		   (map/k get-fbind-formals/k fbinds
		    (lambda(lformals)
		     (map/k get-fbind-body/k fbinds
		      (lambda(bodies)
		       (let ([new-env (extended-rec-env fids lformals bodies e)])
			(eval-ast/k body new-env k))))))))])))

(define map/k
 (lambda (pr ls k)
  (cond
   [(null? ls) (apply-k k ls)]
   [else (pr (first ls) (lambda(v) (map/k pr (rest ls) (lambda(w) (apply-k k (cons v w))))))])))

(define apply-proc/k
  (lambda (pr args k)
    (cases proc pr
	   [make-op (prim sig)
		    (apply-prim-op/k prim sig args k)]
	   [closure (formals body env)
		    (apply-closure/k formals body env args k)])))

(define match-sig/k?
  (lambda (sig? val k)
    (apply-k k (sig? val))))

(define lookup-env/k
  (lambda(e x succ f)
  (cases env e
    [empty-env () (f)]
    [extended-env (ids dvals outer-env) 
    		  (if (memq x ids)
  		  (succ (list-ref dvals (- (length ids) (length (memq x ids)))))
		  (lookup-env/k outer-env x succ f))]
    [extended-rec-env (fids lformals bodies outer-env)
		      (if (memq x fids)
			  (let
			      ([formals (list-ref lformals (- (length fids) (length (memq x fids))))]
			       [body (list-ref bodies (- (length fids) (length (memq x fids))))])
			       (succ (closure formals body e)))
			  (lookup-env/k outer-env x succ f))])))

(define (get-bind-id/k b k)
  (cases bind b
	[make-bind (id-sym dval) (apply-k k id-sym)]))

(define (bind-ast/k b k)
  (cases bind b
	 [make-bind (id-sym dval) (apply-k k dval)]))

(define (get-fbind-fid/k b k)
  (cases fbind b
	 [make-fbind (fid formals body) (apply-k k fid)]))

(define (get-fbind-formals/k b k)
  (cases fbind b
	 [make-fbind (fid formals body) (apply-k k formals)]))

(define (get-fbind-body/k b k)
  (cases fbind b
	 [make-fbind (fid formals body) (apply-k k body)]))


(define apply-prim-op/k
  (lambda (op sig args k)
    (let* ([args-sig (rest sig)]
	   [midType (first args-sig)]
	   [lastType (last args-sig)])
      (cond
       [(and
         (<= (length args-sig) (length args))
	 (midType (list-ref args 0)))
         (map/k (lambda(x next) (match-sig/k? lastType x next)) (rest args)
	  (lambda(ls)
	   (foldl/k (lambda(x y n) (n (and x y))) #t ls
	    (lambda(bool)
	     (if bool (apply-k k (apply op  args)) (error 'apply-prim-op "incorrect number or type of arguments to ~a" op))))))]
	 [else  (error 'apply-prim-op "incorrect number or type of arguments to ~a" op)]))))

(define apply-closure/k
  (lambda (formals body env args k)
    (let ([new-env (extended-env formals args env)])
      (eval-ast/k body new-env k))))

(define foldl/k
 (lambda(f seed ls k)
  (cond
   [(null? ls) (apply-k k seed)]
   [else (f (first ls) seed (lambda(x) (foldl/k f x (rest ls) k)))])))


