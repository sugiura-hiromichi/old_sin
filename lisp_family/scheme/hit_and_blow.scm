(import (scheme base) (scheme write) (scheme time) (scheme small))

(define (assert cnd)
	(if cnd
		#T
		;(error "🫠 Assertion Failed-------------------------\n\tcond: " 'cnd)
		(error "🫠 Assertion Failed-------------------------")
	)
)

(define (irand low high seed)
	(if (> low high)
		(let ((tmp low))
			(set! low high)
			(set! high tmp)
		))
	(let ((cur (current-second)) (range (- high low (- 1))))
		(+ low (modulo (exact (floor (* seed (exp (* seed (- cur (floor cur))))))) range))))

(define (random-list)
	(define (rli answer)
		(let ((seed (irand 0 9 (if (null? answer) (modulo (exact (floor (current-second))) 10) (car answer)))))
			(define random (irand 0 9 (+ 10 seed)))
			(cond
				((eqv? (length answer) 4) answer)
				((memv random answer) (rli answer))
				(else (rli (cons random answer)))
			)
		)
	)
	(rli '())
)

(define (input_one_number)
	(let ((num (read)))
		(cond
			((not (integer? num)) (display "please input integer (0 - 9)\n") #f)
			((<= 0 num 9) num)
			(else (display "range error\n") #f)
		)
	)
)

(define (input_four_number)
	(display "input four numbers > ")
	(let loop ((nums '()))
		(if (eqv? (length nums) 4)
			(reverse nums)
			(let ((num (input_one_number)))
				(cond
					((not num)
						(delete_input_data)
						(input_four_number)
					)
					((member num nums)
						(display "numbers are duplicated")
						(delete_input_data)
						(input_four_number)
					)
					(else (loop (cons num nums)))
				)
			)
		)
	)
)

(define (delete_input_data)
	(let ((c (read-char)))
		(unless (char=? #\newline c)
			(delete_input_data)
		)
	)
)

(define (count_bulls answer input)
	(cond
		((null? answer) 0)
		((eqv? (car answer) (car input)) (+ 1 (count_bulls (cdr answer) (cdr input))))
		(else (count_bulls (cdr answer) (cdr input)))
	)
)

(define (count_same_number answer input)
	(cond
		((null? answer) 0)
		((member (car answer) input) (+ 1 (count_same_number (cdr answer) input)))
		(else (count_same_number (cdr answer) input))
	)
)

(define (display_gameover answer)
	(display "game over... the answer is: ")
	(display answer)
	(newline)
)

(define (display_bulls_cows count answer input bulls)
	(display count)
	(display ": ")
	(display "bulls ")
	(display bulls)
	(display ", cows ")
	(display (- (count_same_number answer input) bulls))
	(newline)
)

(define (play answer)
	(let loop ((count 1))
		(let* ((input (input_four_number)) (bulls (count_bulls answer input)))
			(display_bulls_cows count answer input bulls)
			(cond
				((eqv? bulls 4) (display "🫠🫠🫠🫠🫠🫠🫠🫠🫠🫠🫠"))
				((<= 10 count) (display_gameover answer))
				(else (loop (+ 1 count)))
			)
		)
	)
)

(define answer (random-list))
(display "------------------game start-------------------\n")
(play answer)
