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

(define apply-k
  (lambda(k v)
    (match k
      [(list 'topk) v]
      [(list 'fold fn t gn) (foldl/k fn t v gn)]
      [(list 'apply val saved-k) (apply-proc/k val v saved-k)]
      [(list 'cons val saved-k) (apply-k saved-k (cons val v))]
      [(list 'lookup saved-k) (apply-k saved-k v)]
      [(list 'map/k pr ls saved-k) (map/k pr ls (list 'cons v saved-k))]
      [(list 'if then else e saved-k a) (apply-k (list 'eval-ast/k e saved-k) (if v then else))]
      [(list 'eval-ast/k e saved-k) (eval-ast/k v e saved-k)]
      [(list 'assume binds body e saved-k) (map/k (lambda(x next) (bind-ast/k x (list 'eval-ast/k e next)))
						  binds
						  (list 'newe v e body saved-k))]
      [(list 'newe ids e body saved-k) (let ([new-e (extended-env ids v e)]) (eval-ast/k body new-e saved-k))]
      [(list 'app rands saved-k a e) (map/k (lambda(x next) (eval-ast/k x e next)) rands (list 'apply v saved-k))]		
      [(list 'rec fbinds body e saved-k) (map/k get-fbind-formals/k fbinds (list 'rec2 fbinds v body e saved-k))]
      [(list 'rec2 fbinds fids body e saved-k) (map/k get-fbind-body/k fbinds (list 'rec3 fids v e body saved-k))]
      [(list 'rec3 fids lformals e body saved-k) (let ([new-env (extended-rec-env fids lformals v e)])
							 (eval-ast/k body new-env saved-k))]
      )))


(define eval-ast/k
  (lambda (a e k)
    (cases ast a
      [number (datum) (apply-k k datum)]
      [id-ref-ast (sym) (lookup-env/k e sym (list 'lookup k) (lambda() (format "unbound identifier ~a" sym)))]
      [boolean (datum) (apply-k k datum)]
      [if-ast (condition then else) (eval-ast/k condition e (list 'if then else e k a))]
      [app (rator rands)
            (eval-ast/k rator e (list 'app rands k a e))]
      
      [assume (binds body)
      	      (map/k get-bind-id/k binds (list 'assume binds body e k))]
      [function (formals body) (apply-k k (closure formals body e))]
      [recursive (fbinds body)
      		 (map/k get-fbind-fid/k fbinds (list 'rec fbinds body e k))])))

(define map/k
 (lambda (pr ls k)
  (cond
   [(null? ls) (apply-k k ls)]
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
    (apply-k k (sig? val))))


(define lookup-env/k
  (lambda(e x succ f)
  (cases env e
    [empty-env () (f)]
    [extended-env (ids dvals outer-env) 
    		  (if (memq x ids)
  		  (apply-k succ (list-ref dvals (- (length ids) (length (memq x ids)))))
		  (lookup-env/k outer-env x succ f))]
    [extended-rec-env (fids lformals bodies outer-env)
		      (if (memq x fids)
			  (let
			      ([formals (list-ref lformals (- (length fids) (length (memq x fids))))]
			       [body (list-ref bodies (- (length fids) (length (memq x fids))))])
			       (apply-k succ (closure formals body e)))
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
		(list 'fold (lambda(x y n) (n (and x y))) #t
		      (lambda(bool)
			(if bool (apply-k k (apply op  args)) (error 'apply-prim-op "incorrect number or type of arguments to ~a" op)))))]
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