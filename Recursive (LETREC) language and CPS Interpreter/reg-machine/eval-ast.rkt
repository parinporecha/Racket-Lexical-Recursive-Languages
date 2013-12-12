
#lang racket

(require eopl/eopl)
(require "ast.rkt")
(require "env-rec.rkt")
(require "semantic-domains.rkt")


(provide
  eval-ast/k
  *top-k*)

(define *top-k*
 '(topk))

(define uninitialised '_)

(define *k* uninitialised)
(define *res* uninitialised)

(define goto
  (lambda(thing)
    (thing)))

(define apply-k
  (lambda()
    (match *k*
      [(list 'topk) *res*]
      [(list 'fold fn t gn) (foldl/k fn t *res* gn)]
      [(list 'apply val saved-k) (apply-proc/k val *res* saved-k)]
      [(list 'cons val saved-k) (begin
				  (set! *k* saved-k)
				  (set! *res* (cons val *res*))
				  (goto apply-k))]
      [(list 'lookup saved-k) (begin
				(set! *k* saved-k)
				(goto apply-k))]
      [(list 'map/k pr ls saved-k) (map/k pr ls (list 'cons *res* saved-k))]
      [(list 'if then else e saved-k a) (begin
					      (set! *k* (list 'eval-ast/k e saved-k))
					      (set! *res* (if *res*  then else))
					      (goto apply-k))] 
      [(list 'eval-ast/k e saved-k) (eval-ast/k *res* e saved-k)]
      [(list 'assume binds body e saved-k) (map/k (lambda(x next) (bind-ast/k x (list 'eval-ast/k e next)))
						  binds
						  (list 'newe *res* e body saved-k))]
      [(list 'newe ids e body saved-k) (let ([new-e (extended-env ids *res* e)]) (eval-ast/k body new-e saved-k))]
      [(list 'app rands saved-k a e) (map/k (lambda(x next) (eval-ast/k x e next)) rands (list 'apply *res* saved-k))]		
      [(list 'rec fbinds body e saved-k) (map/k get-fbind-formals/k fbinds (list 'rec2 fbinds *res* body e saved-k))]
      [(list 'rec2 fbinds fids body e saved-k) (map/k get-fbind-body/k fbinds (list 'rec3 fids *res* e body saved-k))]
      [(list 'rec3 fids lformals e body saved-k) (let ([new-env (extended-rec-env fids lformals *res* e)])
							 (eval-ast/k body new-env saved-k))])))


(define eval-ast/k
  (lambda (a e k)
    (cases ast a
      [number (datum) (begin
		(set! *k* k)
		(set! *res* datum)
		(goto apply-k))]
      [id-ref-ast (sym) (lookup-env/k e sym (list 'lookup k) (lambda() (format "unbound identifier ~a" sym)))]
      [boolean (datum)(begin
		(set! *k* k)
		(set! *res* datum)
		(goto apply-k))]
      [if-ast (condition then else) (eval-ast/k condition e (list 'if then else e k a))]
      [app (rator rands)
            (eval-ast/k rator e (list 'app rands k a e))]
      
      [assume (binds body)
      	      (map/k get-bind-id/k binds (list 'assume binds body e k))]
      [function (formals body) (begin
				 (set! *k* k)
				 (set! *res* (closure formals body e))
				 (goto apply-k))]
      [recursive (fbinds body)
      		 (map/k get-fbind-fid/k fbinds (list 'rec fbinds body e k))])))

(define map/k
 (lambda (pr ls k)
  (cond
   [(null? ls)  (begin
		  (set! *k* k)
		  (set! *res* ls)
		  (goto apply-k))]
   [else (pr (first ls) (map1-k pr (rest ls) k))])))

(define map1-k
  (lambda(pr ls k)
    (list 'map/k pr ls k)))

(define apply-proc/k
  (lambda (pr args k)
    (cases proc pr
	   [make-op (prim sig)
		    (apply-prim-op/k prim sig args k)]
	   [closure (formals body env)
		    (apply-closure/k formals body env args k)])))

(define match-sig/k?
  (lambda (sig? val k)
    (begin
      (set! *k* k)
      (set! *res* (sig? val))
      (goto apply-k))))

(define lookup-env/k
  (lambda(e x succ f)
  (cases env e
    [empty-env () (f)]
    [extended-env (ids dvals outer-env) 
    		  (if (memq x ids)  	    
		   (begin
		     (set! *k* succ)
		     (set! *res* (list-ref dvals (- (length ids) (length (memq x ids)))))
		     (goto apply-k))
		   (lookup-env/k outer-env x succ f))]
    [extended-rec-env (fids lformals bodies outer-env)
		      (if (memq x fids)
			  (let
			      ([formals (list-ref lformals (- (length fids) (length (memq x fids))))]
			       [body (list-ref bodies (- (length fids) (length (memq x fids))))])
			    (begin
			      (set! *k* succ)
			      (set! *res* (closure formals body e))
			      (goto apply-k)))
			  (lookup-env/k outer-env x succ f))])))



(define (get-bind-id/k b k)
  (cases bind b
	[make-bind (id-sym dval) (begin
				    (set! *k* k)
				    (set! *res* id-sym)
				    (goto apply-k))]))

(define (bind-ast/k b k)
  (cases bind b
	 [make-bind (id-sym dval) (begin
				    (set! *k* k)
				    (set! *res* dval)
				    (goto apply-k))]))

(define (get-fbind-fid/k b k)
  (cases fbind b
	 [make-fbind (fid formals body)  (begin
						   (set! *k* k)
						   (set! *res* fid)
						   (goto apply-k))]))

(define (get-fbind-formals/k b k)
  (cases fbind b
	 [make-fbind (fid formals body)  (begin
						   (set! *k* k)
						   (set! *res* formals)
						   (goto apply-k))]))

(define (get-fbind-body/k b k)
  (cases fbind b
	 [make-fbind (fid formals body)  (begin
						   (set! *k* k)
						   (set! *res* body)
						   (goto apply-k))]))


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
		(list 'fold (lambda(x y n) (n (and x y))) #t
		      (lambda(bool)
			(if bool 
			    (begin
			      (set! *k* k)
			      (set! *res* (apply op args))
			      (goto apply-k))
			    (error 'apply-prim-op "incorrect number or type of arguments to ~a" op)))))]
      [else  (error 'apply-prim-op "incorrect number or type of arguments to ~a" op)]))))
			
(define apply-closure/k
  (lambda (formals body env args k)
    (let ([new-env (extended-env formals args env)])
      (eval-ast/k body new-env k))))

(define foldl/k
 (lambda(f seed ls k)
  (cond
   [(null? ls) (k seed)]
   [else (f (first ls) seed (lambda(x) (foldl/k f x (rest ls) k)))])))