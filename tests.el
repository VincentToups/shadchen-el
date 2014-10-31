;;; tests.el --- tests for shadchen.el

(require 'shadchen)
(require 'ert)


(ert-deftest shadchen-symbol-match-failed ()
  "Test symbol matching."
  (assert 
   (equal 
    10
    (match 10
           (x x)))
   ()
   "symbol match failed."))

(ert-deftest shadchen-match-string ()
  "Match a string."
  (should
   (equal "test"
          (match "test"
                 ("test" "test")))))

(ert-deftest shadchen-match-string-failed ()
  "Match a string fails."
  (assert 
   (equal :fail
          (match "test"
                 ("dog" :pass)
                 (_ :fail)))))

(ert-deftest shadchen-match-list ()
  "Matching a list."
  (assert 
   (equal 
    (list 1 2 3)
    (match (list 1 2 3)
           ((list x y z)
            (list x y z))))))


;; lexical

(ert-deftest shadchen-match-lexically ()
  "lexical-match symbol"
  (should
   (equal 10
          (let 
              ((f 
                (lexical-match 10 
                               (x 
                                (lambda ()
                                  x)))))
            (funcall f)))))


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

(ert-deftest shadchen-match-plist ()
  "Show plist matching working."
  (assert
   (equal
    (match
     '(:one 1 :two 2 :three 3)
     ((plist :two a) a))
    2))) ; because that's the value of :two in the plist

(ert-deftest shadchen-match-alist ()
  "Show alist matching working."
  (assert
   (equal
    (match
     '((a . 1)(b . 2)(c . 3))
     ((alist 'c a) a))
    3))) ; because that's the value of 'c in the alist

(ert-deftest shadchen-match-struct ()
  ;; First define a struct
  (defstruct shadchen-testpat name version address)
  ;; Now let's test a struct object match with shadchen
  (equal 
   (let ((struct-obj (make-shadchen-testpat
                      :name "test1"
                      :version "0.0.1"
                      :address "test1.example.com")))
     (match struct-obj
       ((struct shadchen-testpat
                name name
                version v) (list name v))))
   (list "test1" "0.0.1")))



;;; Example tests

(ert-deftest shadchen-funcall-find-if-example ()
  "A good example of using `find-if' with funcall."
  ;; We want to match only the first string element from the list...
  (should
   (equal
    (match
     `(1 2 sym "a string" 2 3 4)
     ((funcall (lambda (l) (find-if 'stringp l)) a) a))
    "a string")))

;;; tests.el ends here
