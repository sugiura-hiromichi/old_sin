(import (scheme base) (scheme write) (scheme small))

(define (assert cnd)
	(if cnd
		#T
		;(error "🫠 Assertion Failed-------------------------\n\tcond: " 'cnd)
		(error "🫠 Assertion Failed-------------------------")
	)
)

(define (pow n) (* n n))

(define (cubic n) (* n n n))
(define (half x) (/ x 2))
(define (med x y) (half (+ x y)))
(define (square-med x y) (half (+ (pow x) (pow y))))
(define (sumn n) (half (* n (+ n 1))))

; insert
(define var 10)
(define var0 var)
;this will cause error (define var1)
(assert (eqv? 55 (sumn var)))
(assert (equal? (list var var0) '(10 10)))
(set! var 20)
(assert (eqv? var 20))

(define (modify_global) (set! var 200))
(modify_global)
(assert (eqv? 200 var))

(define var1 'var)
(assert (eqv? var1 'var))
(define var2 '(0 1))
(assert (equal? var2 '(0 1)))

; リスト操作(非破壊的)
(assert (eqv? (car var2) 0))
(assert (equal? (cdr var2) '(1)))

(define dot_pair (cons var var0))
(assert (equal? dot_pair '(200 . 10)))

(define dot_pair (cons var2 var0))
(assert (equal? dot_pair '((0 1) . 10)))

(define appended (append '(0 (1 (2 3) 4))  '(5 6)))
(assert (equal? appended '(0 (1 (2 3) 4) 5 6)))
(define listed (list '(0 (1 (2 3) 4))  '(5 6)))
(assert (equal? listed '((0 (1 (2 3) 4)) (5 6))))

;----------------------------これらは全て非破壊的 💥破壊したい場合は`set!`を使う
(set! appended (cons appended 7))
(assert (equal? appended '((0 (1 (2 3) 4) 5 6) . 7)))
(define xs (cons 'a (list (list 'b) (list (list 'c)) (list (list (list 'd))))))
(assert (equal? xs '(a (b) ((c)) (((d))))))

(define ys (list '(a b c) '(d e f) '(g h i)))
(assert (equal? ys '((a b c) (d e f) (g h i))))

(define zs (list (cons 'a 'b) (cons 'c 'd) (cons 'e 'f)))
(assert (equal? zs '((a . b) (c . d) (e . f))))

(assert (equal? (list-ref zs 2) '(e . f)))

; 条件分岐

(define (select_drink degree)
	(if (> 30 degree)
		"ice\n"
		"hot\n"
	)
)
(assert (equal? (select_drink 0) "ice\n"))
(select_drink 100)

(assert (= 0 0 0 0))
(assert (< 0 1 2 3))

(define (check_num x) (not (and (< 0 x 100) (= (remainder x 3) 0) )))
(define (another_check x) (or (= x 9) (= x 27) (= x 36) (= x 81)))

(define (retrieve n l) (if (< 0 n) (retrieve (- n 1) (cdr l)) (car l)))
(assert (eqv? (retrieve 6 '(6 66 666 6666 66666 666666 6666666)) 6666666))

(define (my_append l1 l2) (if (null? l1) l2 (cons (car l1) (my_append (cdr l1) l2))))
(assert (equal? (my_append '(0 1 2)  '(3 4 5)) '(0 1 2 3 4 5)))

; 型述語
(assert (null? (cdr '(0))))
(assert (number? 0.000))
(assert (integer? 0))
(assert (real? 0.000))
(assert (rational? 1/2))
(assert (complex? 1+i))
(assert (symbol? 'symbol))
(assert (string? "ssssssssssssssss"))
(assert (list? '(1 2 3)))
(assert (pair? (cons 0 1)))

(define (divisor? m n) (if (< m n) (= m 0) (divisor? (- m n) n)))
(define (between n lo high) (< lo n high))
(define (single? x) (null? (car xs)))
(define (double? x) (single? (cdr x)))

; スコープ
(assert (eqv? (let ((a 0)) a) 0))

(let (( y 9 ))
  (let (( z (cond
		((= 0 y) y)
		((= 1 y) y)
		((= 2 y) y)
		((= 3 y) y)
		((= 4 y) y)
		((= 5 y) y)
		((= 6 y) y)
		((= 7 y) y)
		((= 8 y) y)
		(else y)
	)))
	(assert (eqv? z 9)))
)

; 末尾再帰
(define (fact n) (facti n 1))
(define (facti n a) (if (> 1 n) a (facti (- n 1) (* n a))))
(assert (eqv? (fact 4) 24))

(define (my_reverse n)
	(define (reversei n rslt)
		(if (null? n)
			rslt
			(reversei (cdr n) (cons (car n) rslt))
		)
	)
	(reversei n '())
)

(assert (equal? (my_reverse '(0 1 2 3)) '(3 2 1 0)))

(define (fib n)
	(define (fibi n rslt0 rslt1)
		(if (> 2 n)
			rslt1
			(fibi (- n 1) rslt1 (+ rslt0 rslt1))
		)
	)
	(fibi n 0 1)
)
; 0 1 1 2 3 5 8 13 21
(assert (eqv? (fib 7) 13))

(define (fib2 n)
	(let fib2i ((n n) (rslt0 0) (rslt1 1))
		(if (< n 2)
			rslt1
			(fib2i (- n 1) rslt1 (+ rslt0 rslt1)))))

(define (my_length xs)
	(define (ml x l)
		(if (null? x)
			l
			(ml (cdr x) (+ l 1))))
	(ml xs 0))

(define (longer? x y)
	(> (my_length x) (my_length y)))

(define (take x n)
	(define (takei n x rslt)
		(if (or (> 1 n) (null? x))
			rslt
			(takei (- n 1) (cdr x) (cons rslt (car x)))
		)
	)
	(takei n x '())
)

(define (drop x n)
	(if (or (null? x) (< n 0))
		x
		(drop (cdr x) n)))

(define (sum-list xs)
	(define (sum-listi xs rslt)
		(if (null? xs)
			rslt
			(sum-listi (cdr xs) (+ rslt (car xs)))))
	(sum-listi xs 0))

(define (product xs)
	(define (pi xs rslt)
		(if (or (null? xs) (eqv? rslt 0))
			rslt
			(pi (cdr xs) (* rslt (car xs)))))
	(pi xs 1))

#| 数当て |#

; seed
(define seed 1.0)

; seedの設定
(define (srand)
	(let ((cur (current-second)))
	(set! seed (* (/ 1.0 (- cur (round cur))) (exp (- cur (floor cur))))))
	seed)

; 整数の一様乱数
(define (irand low high) (+ low (modulo (exact (floor (srand))) (- high low))))

(define (for n f)
	(f)
	(if (< 0 n) (for (- n 1) f))
)

(for 3 (lambda () (display (irand 1 100)) (display "\n")))

;(display "input:")
;(define ipt (read))
;(display "your input is ")
;(display ipt)

(define (my-make-list n x)
	(if (< 0 n)
		(cons x (my-make-list (- n 1) x))
		'()))

(assert (equal? (my-make-list 3 0) '(0 0 0)))

(define (read-data-list n)
	(define (rdl)
		(display "input: ")
		(read)
	)
	(if (< 0 n)
		(cons (rdl) (read-data-list (- n 1)))
		'()
	)
)
;(display (read-data-list 3))

(define (iota n m)
	(cons n (cond
		((< n m) (iota (+ n 1) m))
		((= n m) '())
		((> n m) (iota (- n 1) m))
	))
)

(assert (equal? (iota 0 2) '(0 1 2)))
(assert (equal? (iota 2 0) '(2 1 0)))
(assert (equal? (iota 5 5) '(5)))
