(assert (eq 24 (* 1 2 3 4)) nil 0)
(defun fib (n)
  "Return the nth Fibonacci number."
  (if (< n 2)
		n
		(+ (fib (- n 1))
		(fib (- n 2)))))

(assert (eq 8 (fib 6)) nil 1)


(equal #'fib (car (list #'fib))); T

(defparameter foo 0)
(defun foo (x) x)

; symbols
(assert (equal (symbol-name 'foo) "FOO"))
(assert (equal (symbol-value 'foo) 0))
(symbol-function 'foo); #<FUNCTION FOO>

(assert (equal 0 (+))); +の単位元は0
(assert (equal 1 (*))); *の単位元は1
(assert (equal 0 (- 0))); -の右単位元は0
(assert (equal 1 (/ 1))); /の右単位元は1

#| these will fails because '- & '/ requires at least one argument
(assert (equal 0 (-)))
(assert (equal 0 (/)))
|#

(assert t)

; List
(assert (equal '(2) '(2)))
(assert (equal (car '(1 2 3 4)) 1)); 要素抽出
(assert (equal (cdr '(1 2 3 4)) '(2 3 4)))
(assert (equal (nth 0 '(1 2 3 4)) 1))
(assert (equal (nth 3 '(1 2 3 4)) 4))
(assert (equal (length '(6 6 6 6 6)) 5)); 長さ
(assert (equal (append '(3 2) '(1)) '(3 2 1))); 連結
(assert (equal (find 2 '(1 2)) 2)); リスト検索
(assert (equal (find 3 '(1 2)) nil))
(assert (equal (member 2 '(1 2)) '(2)))
(assert (equal (member 3 '(1 2)) nil))

(assert (equal (cons 2 3) '(2 . 3))); こんすせる
(assert (equal (cons 2 nil) '(2)))

; array
#(1 2 3)
#(1 2.5 (1 2))

#| this failes
(defparameter two_dim_ary #(1 2 3 4))
(defparameter two_dim_ary2 #(1 2 3 4))
(assert (equal two_dim_ary two_dim_ary2))
|#

(make-array 1); #(0)
(make-array '(1 2)); #2A((0 0))
(make-array '(1 2 3)); #3A(((0 0 0) (0 0 0)))
(make-array 1 :element-type 'string); #("0")

(defparameter ar (make-array '(1 2 3)))
(assert (equal (array-dimensions ar) '(1 2 3)))

(defparameter ar2 #("a" "b"))
(assert (equal (aref ar2 0) "a"))
(defparameter ar3 #2A(("a" "b") ("c" "d")))
(assert (equal (aref ar3 0 1) "b"))
(defparameter ar4 #(("a" "b") ("c" "d")))
(assert (equal (aref ar4 1) '("c" "d")))
(assert (equal (nth 1 (aref ar4 1)) "d"))

(defparameter s (make-array 3 :element-type 'character)); 文字列(string)は(character)の一次元配列(ベクトル)で構成されている
(setf (aref s 0) #\w)
(setf (aref s 1) #\t)
(setf (aref s 2) #\f)
(assert (equal s "wtf"))

; hash table
(defparameter h (make-hash-table))
(setf (gethash '0 h) 0); 値を設定
(setf (gethash '2 h) "1")
(setf (gethash 'last h) #\c)
(assert (equal (gethash '2 h) "1")); 値を取得
(assert (equal (gethash '0 h) 0))
(assert (equal (hash-table-count h) 3)); ハッシュテーブルの個数
(remhash '0 h)
(assert (equal (hash-table-count h) 2))
(clrhash h)
(assert (equal (hash-table-count h) 0))

; struct
(defstruct ObjectOriented member method)

(defparameter cls (make-ObjectOriented :member s :method 'fib))
(assert (equal (ObjectOriented-member cls) "wtf"))
;this failes [illegal function call] (assert (equal ((ObjectOriented-method cls) 10) 55))
(setf (ObjectOriented-method cls) '(2 5 2 5))
(assert (equal (ObjectOriented-method cls) '(2 5 2 5)))

; class
(defclass MyNewGear () ((old :initform 0 :accessor MyNewGear.old :initarg :old) (new :initform 1 :accessor MyNewGear.new :initarg :new)))
(defparameter mng (make-instance 'MyNewGear :old "old"))
(setf (slot-value mng 'old) "m1mba")
(assert (equal (slot-value mng 'old) "m1mba"))
(setf (slot-value mng 'new) "m3mbp")
(assert (equal (MyNewGear.old mng) "m1mba"))

(defmethod say ((hoge MyNewGear)) (concatenate 'string (MyNewGear.old mng) " → " (MyNewGear.new mng)))
(assert (equal "m1mba → m3mbp" (say mng))); この行は`say`メソッドが定義されてから出ないとエラーとなる

(defmethod fib_with_idx ((n integer))
	(if (< n 2)
		n
		(+ (fib_with_idx (- n 1))
			(fib_with_idx (- n 2))
		)
	)
)

(assert (equal (fib 12) (fib_with_idx 12)))

; スペシャル変数(多言語でのグローバル変数に相当)
(defvar *x*); 慣習的にスペシャル変数は変数名を*で囲む
(setf *x* 666)
(assert (equal *x* 666))
(defvar *y* 123)
(assert (equal *y* 123))

; レキシカル変数(他言語でのローカル変数に相当)
(let (x y)
	(setf x 46)
	(setf y 49)
	(assert (equal 4649 (+ y (* x 100))))
)

; 多植代入
(let ((x 0) (y 0))
	(multiple-value-bind (x y) (floor 3.14)
		(assert (equal x 3))
		;this cause error because h is slightly different from actual `0.14` (assert (equal h 0.14))
		(assert
			(<
				(if (< 0 (- y 0.14))
					(- y 0.14)
					(- 0.14 y)
				)
				0.001
			)
		)
	)
	(multiple-value-setq (x y) (floor 2.7182)#|`multiple-value-setq takes no more arguments`|#)
	(defun napier (n)
		(defun fractial (n frc nap limit)
			(if (eql n 0)
				(if (< n limit)
					(fractial 1 1 1 limit)
					1
				)
				(let ((next_n (+ n 1)) (cur_frc (* frc n)) cur_nap)
					(setf cur_nap (+ nap (/ 1 cur_frc)))
					(if (< n limit)
						(fractial next_n cur_frc cur_nap limit)
						cur_nap
					)
				)
			)
		)
		(fractial 0 0 0 n)
	)
	(print (float (napier 50)))
	(assert (< (napier 50) 2.72))
)

; 演算子
(= 0 0)
(/= 0 1); not equal
(< 0 1)
(> 1 0)
(<= 1 1)
(>= 2 1)
(string= "wtf" "wtf")
(string/= "omg" "lol")
(string< "a" "b"); 辞書に出てくる順番で比較
(string> "z" "y")
(string<= "y" "y")
(string>= "z" "y")

; types
(assert (typep 0 'number))
(assert (typep 0 'integer))
(assert (typep 0.0 'float))
(assert (typep 0.0d3 'double-float))
(assert (typep "0" 'string))
(assert (typep #\0 'character))
(assert (typep 'fib 'symbol))
(assert (typep '(0 1 2) 'list))
(assert (typep (cons nil nil) 'cons))
(assert (typep (cons #\0 0) '(cons character integer)))

(print (describe 'symbol))

(deftype my-type () 'integer)
(assert (typep 0 'my-type))

(deftype my-type2 () '(integer 0 9))
(assert (typep 9 'my-type2))

(deftype my-type3 () '(satisfies evenp plusp)); 正の偶数
(deftype my-type4 () '(and integer (satisfies plusp))); 正の整数

; リスト・ベクトル・文字列変換
(assert (equal (aref (coerce '(1 2) 'vector) 1) 2))

; 条件分岐
(when (equal 0 0)
	(assert (equal 0 0))
	(assert (equal 0 0))
	(cond
		((< 0 10) (assert T))
		((= 0 10) (assert T))
		((> 0 10) (assert T))
	)
	(let (( x 0 ))
		(case x
			((1 2) (assert T))
			((3 4 5) (assert T))
			((6 7 8 9) (assert T))
			(otherwise (assert T))
		)
	)
)

(assert (equal (mapcar (lambda (x) (* x 2)) '(0 1 2)) '(0 2 4)))
(assert (equal (mapcar #'fib '(0 1 2 3 4 5)) '(0 1 1 2 3 5)))

(print "-------------------------🫠")
