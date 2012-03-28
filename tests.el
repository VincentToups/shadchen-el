(byte-compile-file "./shadchen.el" t)

;; symbol
(assert 
 (equal 
  10
  (match 10
		 (x x)))
 ()
 "symbol match failed.")

;; string
(assert
 (equal "test"
		(match "test"
			   ("test" "test"))))

;; string-fail
(assert 
 (equal :fail
		(match "test"
			   ("dog" :pass)
			   (_ :fail))))

;; list
(assert 
 (equal 
  (list 1 2 3)
  (match (list 1 2 3)
		 ((list x y z)
		  (list x y z)))))


;; lexical

;; lexical-match symbol

(assert 
 (equal 10
		(let 
			((f 
			  (lexical-match 10 
							 (x 
							  (lambda ()
								x)))))
		  (funcall f))))