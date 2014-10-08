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

(defun-match- test-product (nil acc)
  "Return the accumulator."
  acc)
(defun-match test-product ((list-rest hd tl) acc)
  "Recur, mult. the acc by the hd."
  (recur tl (* hd acc)))
(defun-match test-product (lst)
  "Entry-point: find the product of the numbers in LST."
  (recur lst 1))

(make-defun-match-unbound 'test-product)
(fmakunbound 'test-product)
(setf (symbol-function 'test-product) nil)

(test-product (list 1 2 3))
(eq (get-recur-sigil-for 'x) (get-recur-sigil-for 'x))


;; Alists and Plists

(assert
 (equal
  (match
   '(:one 1 :two 2 :three 3)
   ((plist :two a) a))
  2)) ; because that's the value of :two in the plist

(assert
 (equal
  (match
   '((a . 1)(b . 2)(c . 3))
   ((alist 'c a) a))
  3)) ; because that's the value of 'c in the alist

;; 
