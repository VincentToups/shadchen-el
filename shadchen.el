;;; shadchen.lisp
;; Version: 1.0
;; Author: Vincent Toups
;; Maintainer: Vincent Toups
;; Tags: pattern matching, functional programming
 
;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).

(defstruct match-fail-struct)

(defvar *match-fail* (make-match-fail-struct))
(defvar *shadchen-binding-mode* :dynamic)
(defun adjust-let-for-mode (input)
  (case *shadchen-binding-mode*
	(:lexical (case input
				(let 'lexical-let)
				(let* 'lexical-let*)
				(t 
				 (error "adjust-let-for-mode expects let or let*."))))
	(:dynamic input)))

(let ((*shadchen-binding-mode* :lexical))
  (adjust-let-for-mode 'let)) 

(defun non-keyword-symbol (o)
  (and (symbolp o)
	   (not (keywordp o))))

(defun match-list-expander* (sub-expressions match-value body)
  (cond 
   ((not sub-expressions) `(if (not ,match-value) (progn ,@body) *match-fail*))
   (:otherwise
	(let ((first-expression (car sub-expressions))
		  (list-name (gensym "MATCH-LIST-EXPANDER*-")))
	  `(let ((,list-name ,match-value))
		 (if (and (listp ,list-name)
				  ,list-name)
			 (match1 ,first-expression (car ,list-name)
					 (match1 (list ,@(cdr sub-expressions)) (cdr ,list-name) 
							 ,@body))
		   *match-fail*))))))

(defun match-list-expander (match-expression match-value body)
  (match-list-expander* (cdr match-expression) match-value body))


(defun match-cons-expander (match-expression match-value body)
  (let ((car-match (elt match-expression 1))
		(cdr-match (elt match-expression 2))
		(name (gensym "MATCH-CONS-EXPANDER-")))
	`(let ((,name ,match-value))
	   (if (listp ,name)
		   (match1 ,car-match (car ,name)
				   (match1 ,cdr-match (cdr ,name)
						   ,@body))
		 *match-fail*))))

(defun match-quote-expander (match-expression match-value body)
  `(if (equalp ,match-expression ,match-value) (progn ,@body) *match-fail*))

(defun match-backquote-expander (match-expression match-value body)
  (let ((datum (cadr match-expression)))
	(cond 
	 ((not datum) `(progn ,@body))
	 ((and (listp datum)
		   (eq (car datum) 'uq))
	  (let ((sub-match (cadr datum)))
		`(match1 ,sub-match ,match-value ,@body)))
	 ((listp datum)
	  (let ((first-qt (car datum))
			(rest-bq (cdr datum))
			(name (gensym "MATCH-BACKQUOTE-EXPANDER-")))
		`(let ((,name ,match-value))
		   (if (and ,name
					(listp ,name))
			   (match1 (bq ,first-qt) (car ,name)
					   (match1 (bq ,rest-bq) (cdr ,name) ,@body))
			 *match-fail*))))
	 (:otherwise 
	  `(match1 ',datum ,match-value ,@body)))))

(defun match-and-expander* (sub-expressions match-name body)
  (cond 
   ((not sub-expressions) `(progn ,@body))
   (:otherwise 
	(let ((s1 (car sub-expressions))
		  (name (gensym "MATCH-AND-EXPANDER*-")))
	  `(match1 ,s1 ,match-name 
			   (match1 (and ,@(cdr sub-expressions)) ,match-name ,@body))))))

(defun match-and-expander (match-expression match-value body)
  (let ((name (gensym "MATCH-AND-EXPANDER-")))
	`(let ((,name ,match-value))
	   ,(match-and-expander* (cdr match-expression) name body))))

(defun match-?-expander (match-expression match-value body)
  (let ((name (gensym "MATCH-?-EXPANDER-NAME-"))
		(f-name (gensym "MATCH-?-EXPANDER-FUNCTION-")))
	(case (length (cdr match-expression))
	  (0 (error "MATCH1: MATCH-?-EXPANDER: zero arguments to MATCH-?-EXPANDER.  Needs 1 or 2."))
	  (1 `(let ((,name ,match-value)
				(,f-name ,(cadr match-expression)))
			(if (funcall ,f-name ,name) (progn ,@body) *match-fail*)))
	  (2 `(let ((,name ,match-value)
				(,f-name ,(cadr match-expression)))
			(if (funcall ,f-name ,name) (match1 ,(elt match-expression 2) ,name ,@body)
			  *match-fail*)))
	  (otherwise
	   (error "MATCH-?-EXPANDER: MATCH-?-EXPANDER takes only 1 or 2 arguments.")))))

(defun match-values-expander (match-expression match-value body)
  (let ((name (gensym "MATCH-VALUES-EXPANDER-")))
	`(let ((,name (multiple-value-list ,match-value)))
	   (match1 (list ,@(cdr match-expression)) ,name ,@body))))

(defun match-funcall-expander (match-expression match-value body)
  (assert (and (listp match-expression) (= 3 (length match-expression)))
		  (match-expression)
		  "MATCH-FUNCALL-EXPANDER: FUNCALL match expression must have
two terms, a function and a match against the result.  Got
%s." match-expression)
  (let ((name (gensym "MATCH-FUNCALL-EXPANDER-NAME-"))
		(fun-name (gensym "MATCH-FUNCALL-EXPANDER-FUN-NAME-"))
		(result-name (gensym "MATCH-FUNCALL-EXPANDER-RESULT-NAME-")))
	`(let* ((,name ,match-value)
			(,fun-name ,(cadr match-expression))
			(,result-name (funcall ,fun-name ,name)))
	   (match1 ,(caddr match-expression) ,result-name ,@body))))

(defvar *extended-patterns* (make-hash-table) "Holds user declared patterns.")
(defun extended-patternp (pattern-head) 
  "Return T if PATTERN-HEAD indicates a user provided pattern."
  (gethash pattern-head *extended-patterns*))

(defun match-extended-pattern-expander (match-expression match-value body)
  (let* ((pattern-args (cdr match-expression))
		 (pattern-fun (gethash (car match-expression) *extended-patterns*))
		 (expansion (apply pattern-fun pattern-args)))
	`(match1 ,expansion ,match-value ,@body)))

(defmacro* defpattern (name args &body body)
  `(setf (gethash ',name *extended-patterns*)
		 #'(lambda ,args ,@body)))

(defun match-literal-string (match-expression match-value body)
  `(if (string= ,match-expression ,match-value) 
	   (progn ,@body)
	 *match-fail*))

(defun match-literal-number (match-expression match-value body)
  `(if (= ,match-expression ,match-value)
	   (progn ,@body)
	 *match-fail*))

(defun match-literal-keyword (match-expression match-value body)
  `(if (eq ,match-expression ,match-value)
	   (progn ,@body)
	 *match-fail*))

(defun match-let-expander (match-expression match-value body)
  `(,(adjust-let-for-mode 'let) ,(cdr match-expression) ,@body))

(defun match-or-expander (match-expression match-value body)
  (cond 
   ((length=1 (cdr match-expression))
    `(match1 ,(cadr match-expression) ,match-value ,@body))
   (:otherwise
	(let* ((forms (cdr match-expression))
		   (form (car forms))
		   (rest (cdr forms))
		   (nm (gensym "MATCH-OR-EXPANDER-NM-")))
	  `(let* ((,nm ,match-value)
			  (result (match1 ,form ,nm ,@body)))
		 (if (not (eq *match-fail* result))
			 result
		   (match1 (or ,@rest) ,nm ,@body)))))))

(defun shadchen:mapcat (f lst)
  "Concatenate the results of applying f to each element in lst."
  (loop for item in lst append (funcall f item)))

(defun calc-pattern-bindings-extended (expr)
  "Calculate the bound symbols of a user defined pattern."
  (let* ((pattern-args (cdr expr))
		 (pattern-fun (gethash (car expr) *extended-patterns*))
		 (expansion (apply pattern-fun pattern-args)))
	(calc-pattern-bindings-extended expansion)))

(defun calc-backquote-bindings (expr)
  "Calculate the bindings for a backquote expression."
  (loop for sub in (cdr expr) 
		when (and (listp sub)
				  (eq (car sub) 'uq))
		append 
		(calc-pattern-bindings (cadr sub))))

(defun calc-pattern-bindings (expr)
  "Given a shadchen pattern EXPR return a list of symbols bound
by that expression."
  (cond 
   ((non-keyword-symbol expr)
	(list expr))
   ((or (not expr)
		(symbolp expr)
		(numberp expr)
		(stringp expr)) nil)
   ((extended-patternp (car expr))
	(calc-pattern-bindings-extended expr))
   ((listp expr)
	(case (car expr)
	  (quote nil)
	  ((list and values) 
	   (shadchen:mapcat #'calc-pattern-bindings (cdr expr)))
	  (cons (append (calc-pattern-bindings (car expr))
					(calc-pattern-bindings (cdr expr))))
	  ((? funcall) (if (= 2 (length expr)) nil
					 (calc-pattern-bindings (elt expr 2))))
	  (or (calc-pattern-bindings (cadr expr)))
	  (bq (calc-backquote-bindings expr))
	  (let (mapcar #'car (cdr expr)))))
   (:otherwise 
	(error "calc-pattern-bindings: unrecognized pattern %S." expr))))

(defmacro* match1 (match-expression match-value &body body)
  (cond 
   ((not match-expression)
	(match-list-expander '(list) match-value body))
   ((non-keyword-symbol match-expression)
	`(,(adjust-let-for-mode 'let) ((,match-expression ,match-value))
	  ,@body))
   ((stringp match-expression) 
	(match-literal-string match-expression match-value body))
   ((numberp match-expression)
	(match-literal-number match-expression match-value body))
   ((keywordp match-expression)
	(match-literal-keyword match-expression match-value body))
   ((extended-patternp (car match-expression)) 
	(match-extended-pattern-expander match-expression match-value body))
   ((listp match-expression)
	(if match-expression 
		(case (car match-expression)
		  (list (match-list-expander match-expression match-value body))
		  (cons (match-cons-expander match-expression match-value body))
		  (quote (match-quote-expander match-expression match-value body))
		  (and (match-and-expander match-expression match-value body))
		  ((? p) (match-?-expander match-expression match-value body))
		  (funcall (match-funcall-expander match-expression match-value body))
		  (or (match-or-expander match-expression match-value body))
		  (bq (match-backquote-expander match-expression match-value body))
		  (let (match-let-expander match-expression match-value body))
		  (values (match-values-expander match-expression match-value body)))
	  (match-list-expander '(list) match-value body)))
   (:otherwise (error "MATCH1: Unrecognized match expression: %s." match-expression))))

(defmacro* match-helper (value &body forms)
  (assert (symbolp value)
		  (value)
		  "MATCH-HELPER: VALUE must be a symbol!  Got %s." value)
  (cond 
   ((not forms) `(error "No Match for %s!" ,value))
   ((listp forms)
	(let ((first-form (car forms)))
	  (assert (and (listp first-form)
				   (> (length first-form) 1))
			  (first-form)
			  "Each MATCH SUB-FORM must be at least two elements long, a matcher
and an expression to evaluate on match. Got %s instead." first-form)
	  (let ((match-expression (car first-form))
			(match-body-exprs (cdr first-form))
			(result-name (gensym "MATCH-HELPER-RESULT-NAME-")))
		`(let ((,result-name 
				(match1 ,match-expression ,value ,@match-body-exprs)))
		   (if (not (eq *match-fail* ,result-name)) ,result-name
			 (match-helper ,value ,@(cdr forms)))))))))


(defmacro* match (value &body forms)
  "Attempt to match VALUE against each of the patterns in the CAR of
FORMS.  When a match is detected, its subsequent forms are executed as
in a PROGN where the bindings implied by the match are in effect.  

An error is thrown when no matches are found."
  (let ((name (gensym "MATCH-VALUE-NAME-")))
	`(let ((,name ,value)) 
	   (match-helper ,name ,@forms))))

(defmacro* lexical-match (value &body forms)
  "Attempt to match VALUE against each of the patterns in the CAR of
FORMS.  When a match is detected, its subsequent forms are executed as
in a PROGN where the bindings implied by the match are in effect.  

An error is thrown when no matches are found.  Bindings are
lexical via cl.el's lexical let.  An alternative is to use Emacs
24's lexical binding mode and use regular match."
  (let ((*shadchen-binding-mode* :lexical))
	(macroexpand-all `(match value ,@forms))))

(defmacro* match-lambda (&body forms) 
  "Like MATCH except the VALUE is curried."
  (let ((name (gensym "MATCH-LAMBDA-NAME-")))
	`(function (lambda (,name) (match ,name ,@forms)))))

(defun length=1 (lst)
  "Returns T when LST has one element."
  (and (consp lst)
	   (not (cdr lst))))

(defun length=0 (lst)
  "Returns T when LST has one element."
  (and (consp lst)
	   (not lst)))

(defpattern list-rest (&rest patterns)
  (if (length=1 patterns)
	  `(? #'listp ,(car patterns))
	(let ((pat (car patterns))
		  (pats (cdr patterns)))
	  `(and (funcall #'car ,pat)
			(funcall #'cdr 
					 (list-rest ,@pats))))))

(defun cl-struct-prepend (s)
  (intern (format "cl-struct-%s" s)))

(defun make-cl-struct-accessor (struct-name slot) 
  (intern (format "%s-%s" struct-name slot)))


(defpattern struct (struct-name &rest fields)
  `(and
	(? #'vectorp)
	(? #'(lambda (x) (> (length x) 0)))
	(? #'(lambda (o)
		   (eq (elt o 0) ',(cl-struct-prepend struct-name))))
	,@(loop for f in fields collect
			`(funcall 
			  #',(make-cl-struct-accessor struct-name (car f))
			  ,(cadr f)))))

(defpattern let1 (symbol value) 
  `(let (,symbol ,value)))

(defpattern vector@-no-bounds/type-check (index pattern)
  `(funcall 
	#'(lambda (v)
		(aref v ,index))
	,pattern))

(defpattern vector@ (index pattern)
  (let ((ix (gensym "vector@-ix"))
		(v (gensym "vector@-v")))
	`(and 
	  (? #'vectorp ,v)
	  (let1 ,ix ,index)
	  (? #'(lambda (v)
			 (< (length v) ,ix)))
	  (vector@-no-bounds/type-check ,ix ,v))))

(defpattern one-of (pattern)
  `(and
	(? #'listp)
	(funcall #'length (? (lambda (x) (> x 0))))
	(or (funcall #'car ,pattern)
		(funcall #'cdr (one-of ,pattern)))))

(defpattern one-of-two-lists (pattern)
  `(and
	(? #'consp)
	(or (funcall #'car (one-of ,pattern))
		(funcall #'cdr (one-of ,pattern)))))


(defun rotate-list (lst)
  (reverse (cons (car lst) (reverse (cdr (copy-list lst))))))

(defpattern list% (&rest patterns)
  (cond
   ((not patterns) `(? (lambda (x) (eq nil x))))
   (:otherwise
	(let ((pattern1 (car patterns))
		  (rest-patterns (cdr patterns)))
	  `(and 
		(? #'listp)
		(? (lambda (x) (message (format "%s" x)) (> (length x) 0)))
		(or 
		 (and
		  (funcall #'car ,pattern1)
		  (funcall #'cdr (list% ,@rest-patterns)))
		 (list% ,@(rotate-list patterns))))))))

(defpattern list%+ (&rest patterns)
  (cond
   ((not patterns) `(? #'listp))
   (:otherwise
	(let ((pattern1 (car patterns))
		  (rest-patterns (cdr patterns)))
	  `(and 
		(? #'listp)
		(? (lambda (x) (message (format "%s" x)) (> (length x) 0)))
		(or 
		 (and
		  (funcall #'car ,pattern1)
		  (funcall #'cdr (list% ,@rest-patterns)))
		 (funcall #'rotate-list (list% ,@patterns))))))))

(defmacro* match-let ((&rest binders) &body body)
  "Like let but the left-hand-side of each binder pair can be a
shadchen-pattern.  Within a match-let body, the phrase `(recur
arg1 ...)  can be used to trigger a trampolined re-entry into the
match, but only in tail position.  

At the moment, this is not checked at compile time, so unexpected
results can occur if `recur` is used in another position."
  (let ((patterns (mapcar #'car binders))
		(recursion-sigil (gensym "recursion-sigil-"))
		(recur-args (gensym "recur-args-"))
		(recur-results (gensym "recur-results-"))
		(final-result (gensym "final-result-"))
		(value-expressions
		 (mapcar #'cadr binders)))
	`(labels ((recur (&rest ,recur-args)
					 (cons ',recursion-sigil ,recur-args)))
	   (loop with ,recur-results = 
			 (match (list ,@value-expressions)
					((list ,@patterns)
					 ,@body))
			 while (and (listp ,recur-results)
						(eq (car ,recur-results) ',recursion-sigil))
			 do 
			 (setq ,recur-results 
				   (match (cdr ,recur-results)
						  ((list ,@patterns)
						   ,@body)))
			 finally 
			 (return ,recur-results)))))

(defmacro* lexical-match-let ((&rest binders) &body body)
  "Like let but the left-hand-side of each binder pair can be a
shadchen-pattern.  Within a match-let body, the phrase `(recur
arg1 ...)  can be used to trigger a trampolined re-entry into the
match, but only in tail position.  

At the moment, this is not checked at compile time, so unexpected
results can occur if `recur` is used in another position.

Bindings are lexical via cl.el's lexical-let.  An alternative is
to use Emacs 24 & >'s lexical binding mode with regular match-let."
  (let ((patterns (mapcar #'car binders))
		(recursion-sigil (gensym "recursion-sigil-"))
		(recur-args (gensym "recur-args-"))
		(recur-results (gensym "recur-results-"))
		(final-result (gensym "final-result-"))
		(value-expressions
		 (mapcar #'cadr binders)))
	`(labels ((recur (&rest ,recur-args)
					 (cons ',recursion-sigil ,recur-args)))
	   (loop with ,recur-results = 
			 (lexical-match (list ,@value-expressions)
							((list ,@patterns)
							 ,@body))
			 while (and (listp ,recur-results)
						(eq (car ,recur-results) ',recursion-sigil))
			 do 
			 (setq ,recur-results 
				   (match (cdr ,recur-results)
						  ((list ,@patterns)
						   ,@body)))
			 finally 
			 (return ,recur-results)))))

(defvar *match-function-table* (make-hash-table))
(defvar *match-function-doc-table* (make-hash-table))

(defun match-fboundp (symbol)
  "Returns T when symbol is a function and a match function."
  (and (fboundp symbol)
	   (gethash symbol *match-function-table*)))


(defun extend-match-function (symbol function pattern &optional doc)
  "Extends the match function represented by symbol with the function FUNCTION."
  (let ((functions (gethash symbol *match-function-table*)))
	(puthash symbol (reverse (cons function functions)) *match-function-table*)
	(if doc 
		(let ((current-doc (gethash symbol *match-function-doc-table* "")))
		  (puthash symbol 
				   (concat current-doc
						   (format "\n%S : %s" pattern doc))
				   *match-function-doc-table*)))
	symbol))

(eval-when-compile 
  (defmacro* shadchen:let/named (name bindings &body body)
	(let ((arg-names (mapcar #'car bindings))
		  (init-vals (mapcar #'cadr bindings))
		  (results (gensym "results-"))
		  (once (gensym "once-"))
		  (done (gensym "done-"))
		  (sigil (gensym "let/named-sigil-")))
	  `(labels ((,once ,arg-names ,@body)
				(,name ,arg-names 
					   (list ',sigil ,@arg-names)))
		 (loop with 
			   ,results = (,once ,@init-vals) 
			   while (and (listp ,results)
						  (eq (car ,results)
							  ',sigil)) do
							  (setq ,results (apply #',once (cdr ,results)))
							  finally (return ,results))))))

(defun make-defun-match-unbound (name)
  "Make the match function represented by the symbol NAME unbound."
  (puthash name nil *match-function-doc-table*)
  (puthash name nil *match-function-table*)
  (fmakunbound name))

(defvar *shadchen-recur-sigils* (make-hash-table) "Ensure
different bodies of the same shadchen pattern function get the
same recursion markets.")

(defun get-recur-sigil-for (function-name)
  "Fetch the recursion sigil for the match function function-name."
  (let ((s (gethash function-name *shadchen-recur-sigils*)))
	(if s s
	  (prog1
		  s
		(puthash 
		 function-name 
		 (gensym (format "defun-match-recur-sigil-for-%s-" function-name))
		 *shadchen-recur-sigils*)))))

(defmacro* defun-match- (name pattern &body body)
  "Identical to defun-match- except makes any previous
defun-match definitions unbound before defun'ing the function."
  `(progn 
	 (make-defun-match-unbound ',name)
	 (defun-match ,name ,pattern ,@body)))

(defmacro* defun-match (name patterns &body body)
  "Create a function which dispatches to various bodies via
pattern matching.  Multiple bodies can be specified across
several invokations of 'defun-match' and the matching body will
be executed (patterns are checked in the order of definition.)

If no patterns match, then an error indicating the failure of a
match is raised.

If the first item in BODY is a string, it is added to the
documentation for the whole function, along with the associated
pattern."
  (let ((args (gensym "defun-match-arg-list-"))
		(inner-arg (gensym "defun-match-inner-args-"))
		(true-body body)
		(doc (if (stringp (car body)) (car body) ""))
		(fun (gensym "defun-match-fun-"))
		(val (gensym "defun-match-val-"))
		(result (gensym "defun-match-result-"))
		(recur (gensym "recur-point-"))
		(inner-recur-args (gensym "defun-match-inner-recur-args-"))
		(inner-recur-sigil (get-recur-sigil-for name))
		(possibles (gensym "defun-match-possibles-")))
	`(progn 
	   (extend-match-function ',name 
							  (lambda (,inner-arg)
								(match1 (list ,@patterns) ,inner-arg ,@true-body))
							  ',patterns
							  ,doc)
	   (defalias ',name (lambda (&rest ,args)
						  (flet ((recur (&rest ,inner-recur-args)
										(list ',inner-recur-sigil ,inner-recur-args)))
							(shadchen:let/named 
							 ,recur
							 ((,possibles (gethash ',name *match-function-table*)))
							 (cond 
							  ((not ,possibles) (error "Match fail for matching defun %s with arguments %S." ',name ,args))
							  (:otherwise
							   (let* ((,fun (car ,possibles))
									  (,val (funcall ,fun ,args)))
								 (cond 
								  ((eq *match-fail* ,val)
								   (,recur (cdr ,possibles)))
								  ((and (listp ,val)
										(eq (car ,val) ',inner-recur-sigil))
								   (setq ,args (cadr ,val))
								   (,recur (gethash ',name *match-function-table*)))
								  (:otherwise ,val))))))))
		 (gethash ',name *match-function-doc-table*)))))

(defpattern simple-concat (&rest patterns)
  (cond 
   ((length=0 patterns)
	"")
   ((length=1 patterns)
	`(? #'stringp ,(car patterns)))
   (:otherwise
	(let* ((the-string (car patterns))
		   (static-len (length the-string)))
	  `(and 
		(p #'stringp)
		(p (lambda (s)
			 (>= (length s) ,static-len)))
		(p 
		 (lambda (s)
		   (string= (substring s 0 ,static-len) ,the-string)))
		(funcall (lambda (s)
				   (substring s ,static-len))
				 (concat ,@(cdr patterns))))))))


(defpattern full-concat (pivot &rest patterns)
  (assert (numberp pivot)
		  ()
		  "Pivot should be a number.")
  (cond 
   ((length=0 patterns)
	"")
   ((length=1 patterns)
	`(? #'stringp ,(car patterns)))
   (:otherwise
	`(and 
	  (p (lambda (s)
		   (>= (length s) ,pivot)))
	  (or 
	   (and (funcall
			 (lambda (s)
			   (substring s 0 ,pivot))
			 ,(car patterns))
			(funcall 
			 (lambda (s)
			   (substring s ,pivot))
			 (concat ,@(cdr patterns))))
	   (full-concat ,(+ pivot 1) ,@patterns))))))


(defpattern concat (&rest patterns)
  (cond 
   ((length=0 patterns)
	"")
   ((length=1 patterns)
	`(? #'stringp ,(car patterns)))
   (:otherwise
	(cond 
	 ((stringp (car patterns))
	  `(simple-concat ,@patterns))
	 (:otherwise 
	  `(full-concat 0 ,@patterns))))))

(provide 'shadchen)

;;; shadchen.lisp ends here