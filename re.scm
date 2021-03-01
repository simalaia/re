#! /usr/bin/env -S chibi-scheme -r -A ./ -I ${modules}/self/schemeR7RS

(import (chibi) (chibi match)
	(lib misc)
	(scheme cxr) (only (scheme base) symbol=?)
	)

(define empty `(()) )
(define nxt cdr)
(define cur car)
(define (sar s) (substring 0 1 s) )
(define (sdr s) (substring 1 (string-length s) s) )

(define (tp p)   (car p) )
(define (fn p c) ((cadr p) p c) )
(define (ch p)   (caddr p) )
(define (lf p)   (cadddr p) )
(define (rt p)   (cadddr (cdr p)) )

;; (c . n)
;; c is a list of nodes
;; n is a list of nodes

(define (fn-fail p . c) (dspl "match failed") empty )
(define (fn-succ p . c) (dspl "match succeeded") empty )
(define (fn-pass p c) `((,(lf p) ,(rt p))) )

(define (fn-cat  p c) (if (string=? (ch p) c) `(() ,(lf p)) empty ) )
(define (fn-alt  p c) `((,(lf p) ,(rt p))) )
(define (fn-neg  p c)
	(let ( (r (fn (lf p) c)) )
		(dspl r )
		(cond
			((null? (nxt r)) `(() ,(rt p)) )
			((symbol=? (tp (car (nxt r))) `end) (fn-fail `()) )
			(#t `(() (neg ,fn-neg "" ,(car (nxt r)) ())) )) ))

(define (cat c n) `(cat ,fn-cat ,c ,n ()) )
(define (alt l r) `(alt ,fn-alt "" ,l ,r) )
(define (neg l r) `(neg ,fn-neg "" ,l ,r) )
(define (fai) `(end ,fn-fail "" () ()) )
(define (suc) `(end ,fn-succ "" () ()) )

(define (matcher n s)
	(let f ( (c (sar s)) (s (sdr s)) )
		(let l ( (c n) (n `()) )
			(if (null? c) '() '())
			(let ((r 3)) #f) )) )

(define k (cat "d" (suc)))
(define m (alt (cat "c" (cat "a" (cat "t" (suc))) (pass) )) )
(define l (neg (cat "c" (cat "a" (cat "t" (suc)))) (cat "z" (suc))) )

(define (main args)
	(dspl "test: (con (neg cat) z)" )
	(dspl "init:  " (tp l) " " (tp (lf l)) "-" (ch (lf l)) " " (tp (rt l)) "-" (ch (rt l)))
	(let ( (r (fn l "d")) )
		(dspl r ) )
	(dspl "\ntest: (con (neg cat) z)" )
	(let ( (r (fn l "c")) )
		(dspl r ) )
	(dsp ""))