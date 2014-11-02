;;; shadchen.el --- pattern matching for elisp

;; Version: 1.4
;; Author: Vincent Toups
;; Maintainer: Vincent Toups
;; Tags: pattern matching, functional programming
;; Contributors: Nic Ferrier

;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).

;;; Commentary: 
;; Shadchen: A pattern matching library
;; ====================================
;;
;;     shadchen: Noun
;;       matchmaker
;;     from Yiddish
;;
;; (note: there is an emacs lisp port of this library
;;  [here][shadchen-el])
;; (note: if you are reading this README for the emacs version of the
;;  library, keep in mind that emacs symbols are case sensitive.  Symbols
;;  are all lowercase in this library.)
;;
;;
;; I love pattern-matching, which I find to be a great way to combine
;; destructuring data with type-checking when used in dynamic languages.
;; If you aren't familiar with how pattern matching works, here is an
;; example:
;;
;;     (defun second (lst)
;;      (match lst 
;;       ((cons _ (cons x rest)) x)))
;;
;; `MATCH` introduces a pattern matching expression, which takes a value,
;; in this case `LST` and a series of lists, whose first elements are
;; descriptions of a data structure and whose subsequent elements are
;; code to execute if the match succeeds.  Pattern matching takes the
;; description of the data and binds the variables that appear therein to
;; the parts of the data structure they indicate.  Above, we match `_` to
;; the `car` of a list, `x` to the `car` of that list's `cdr`, and `rest`
;; to the `cdr` of that list.  
;;
;; If we don't pass in a list, the match fails.  (Because of the behavior
;; of CL's `car` and `cdr`, which return `NIL` on `NIL`, the form `cons`
;; doesn't enforce a length requirement on the input list, and will
;; return `NIL` for an empty list.  This corresponds with the fact that
;; in Common Lisp `(car nil)` is `nil` and `(cdr nil)` is `nil`.)
;;
;; We might instead write:
;;
;;     (defun second-of-two (lst)
;;       (match lst
;;         ((list _ x) x)))
;;
;; Which returns the second element of a list _only_ when a two element
;; list is passed in.  `MATCH` can take multiple pattern/body sets, in
;; which case patterns are tried in order until one pattern matches, and
;; the result of evaluating the associated forms is returned.  If no
;; patterns match, an error is raised.
;;
;; Built-in Patterns
;; -----------------
;;
;; Shadchen supports the following built-in patterns.
;;
;;     <SYMBOL>
;;
;; Matches anything, binding <SYMBOL> to that value in the body
;; expressions.
;;
;;     <KEYWORD-LITERAL> 
;;
;; Matches only when the value is the same keyword.
;;
;;     <NUMBER-LITERAL>
;;
;; Matches only when the value is the same number.
;;
;;     <STRING-LITERAL>
;;
;; Matches only when the value is `string=` is the same string.	
;;
;;     (CONS <PATTERN1> <PATTERN2>)
;;
;; Matches any `CONS` cell, or `NIL`, then matches `<PATTERN1>` and
;; `<PATTERN2>`, executing the body in a context where their matches are
;; bound.  If the match value is NIL, then each `PATTERN` matches against
;; NIL.
;;
;;     (LIST <P1> ... <PN>)
;;
;; Matches a list of length N, then matches each pattern `<PN>` to the
;; elements of that list.
;;
;;     (LIST-REST <P1> ... <PN> <REST-PATTERN>)
;;
;; Matches <P1> - <PN> to elements in at list, as in the `LIST` pattern.
;; The final `<REST-PATTERN>` is matched against the rest of the list.
;;
;;     (LIST* <P1> ... <PN> <REST-PATTERN>)
;;
;; LIST* is an alias for LIST-REST.
;;
;;     (PLIST key <pattern> ...)
;; 
;; Matches a plist by matching each <pattern> against the key it is paired with.
;;
;;     (ALIST key <pattern> ...)
;; 
;; Matches an alist by matching each <pattern> against the key it is paired with.
;;
;;     (QUOTE DATUM)
;;
;; Only succeeds when `DATUM` is `EQUALP` to the match-value.  Binds no
;; values.
;;
;;      (AND <P1> .. <PN>)
;;
;; Tests all `<PN>` against the same value, succeeding only when all
;; patterns match, and binding all variables in all patterns.
;;
;;      (OR <P1> .. <PN>)
;;
;; Tries each `<PN>` in turn, and succeeds if any `<PN>` succeeds.  The
;; body of the matched expression is then executed with that `<PN>'s`
;; bindings.  It is up to the user to ensure that the bindings are
;; relevant to the body.
;;
;;      (? PREDICATE <PATTERN>)
;;
;; Succeeds when `(FUNCALL PREDICATE MATCH-VALUE)` is true and when
;; `<PATTERN>` matches the value.  Body has the bindings of `<PATTERN>`.  
;;
;;      (FUNCALL FUN <PATTERN>)
;;
;; Applies `FUN` to the match value, then matches `<PATTERN>` against _the
;; result_.
;;
;;      (BQ EXPR)
;;
;; Matches as if by `BACKQUOTE`.  If `EXPR` is an atom, then this is
;; equivalent to `QUOTE`.  If `EXPR` is a list, each element is matches
;; as in `QUOTE`, unless it is an `(UQ <PATTERN>)` form, in which case it
;; is matched as a pattern.  Eg:
;;
;;     (match (list 1 2 3)
;;       ((BQ (1 (UQ x) 2)) x)) 
;;
;; Will succeed, binding `X` to 2.  
;;
;;     (match (list 10 2 20)
;;        ((BQ (1 (UQ x) 2)) x))
;;
;; Will fail, since `10` and `1` don't match.
;;
;;     (values <P1> ... <PN>)
;;
;; Will match multiple values produced by a `(values ...)` form.
;;
;;     (let (n1 v1) (n2 v2) ... (nn vn))
;;
;; Not a pattern matching pattern, per se.  `let` always succeeds and
;; produces a context where the bindings are active.  This can be used to
;; provide default alternatives, as in:
;;
;;     (defun not-nil (x) x)
;;
;;     (match (list 1) 
;;      ((cons hd (or (? #'non-nil tl)
;;                    (let (tl '(2 3)))))
;;       (list hd tl)))
;;
;; Will result in `(1 (2 3))` but 
;;
;;     (match (list 1 4) 
;;      ((cons hd (or (? #'non-nil tl)
;;                    (let (tl '(2 3)))))
;;       (list hd tl)))
;;
;; Will produce `(1 (4))`.  Note that a similar functionality can be
;; provided with `funcall`.
;;
;;     (concat P1 ... PN)
;;
;; Concat is a powerful string matching pattern.  If each pattern is a
;; string, its behavior is simple: it simply matches the string that is
;; the concatenation of the pattern strings.  
;;
;; If any of the patterns are a more complex pattern, then, starting from
;; the left-most pattern, the shortest substring matching the first
;; pattern is matched, ad then matching proceeds on the subsequent
;; patterns and the unmatched part of the string.  Eg:
;;
;;     (match "bobcatdog" 
;;      ((concat 
;;        (and (or "bobcat" "cat") which) 
;;        "dog") which))
;;
;; will produce "bobcat", but the pattern will also match "catdog",
;; returning "cat".
;;
;; This is a handy pattern for simple parsers.
;;
;;     (append P1 ... PN)
;;
;; Like `concat` except for lists rather than strings:
;;
;;     (match
;;        (number-sequence 1 10)
;;      ((append (list 1) _ (list y)) y))  => 10
;;
;; the interveening numbers are matched away.
;;
;; Match-let
;; ---------
;;
;; Match let is a form which behaves identically to a let expression
;; with two extra features: first, the each variable can be an arbitrary
;; shadchen pattern and secondly, one can invoke `recur` in any tail
;; position of the body to induce a trampolined re-entry into the let
;; expression, so that self-recursive loops can be implemented without
;; blowing the stack.
;;
;; eg:
;;
;;     (match-let 
;;      (((list x y) (list 0 0)))
;;      (if (< (+ x y) 100)
;;          (recur (list (+ x 1) (+ y x)))
;;        (list x y)))
;;
;; Will result in `(14 91)`.
;;
;; If you like this feature, please let me know if you would like it to
;; check that `recur` is in tail position.  This is an expensive step
;; which requires walking the body after macro-expansion, which may also
;; introduce subtle bugs.  The upside of doing this is that you avoid the
;; possibly strange bugs encountered when `recur` is invoked in a
;; non-tail position.
;;
;; User feedback will vary how I approach this. 
;;
;; defun-match
;; -----------
;;
;; This special form allows the definition of functions using pattern
;; matching where bodies can be specified over multiple `defun-match`
;; invokations:
;;
;;
;;     (defun-match- product (nil)
;;        "The empty product."
;;        1)
;;  
;;     (defun-match product (nil acc)
;;        "Recursion termination."
;;        acc)
;;  
;;     (defun-match product 
;;         ((cons (p #'numberp n)
;;                (p #'listp rest))
;;          (p #'numberp acc))
;;        "Main body of the product function."
;;        (recur rest (* n acc)))
;;  
;;     (defun-match product (lst)
;;        "Calculate the product of the numbers in LST."
;;        (recur lst 1))
;;
;; Note that different bodies can `recur` to eachother without growing
;; the stack.  Documentation for each body is accumulated, along with the
;; pattern associated with the body, into the function's complete
;; documentation.
;;
;;
;;
;; Extending shadchen
;; ------------------
;;
;; Users can define their own patterns using the `defpattern` form.  For
;; instance, the behavior of `CONS`, which matches the empty list, may
;; not be desired.  We can define a match which doesn't have this
;; behavior as:
;;
;;     (defun non-nil (x) x)
;;     (defpattern cons* (car cdr)
;;      `(? #'non-nil (cons ,car ,cdr)))
;;
;; A pattern is a function which takes the arguments passed into the
;; custom pattern, and expands them into a new pattern in the language of
;; the built-in pattern-matching.  
;;
;; We can now say:
;;
;;     (match (cons 10 11)
;;      ((cons* a b) a)) 
;;
;; Which will produce 10, but:
;;
;;     (match nil
;;      ((cons* a b) a))
;;
;; Will raise a no-match error.  
;;
;; Judicious application of the matchers `AND`, `FUNCALL`, and `?` allow
;; the definition of arbitrary matchers without exposing the guts of the
;; matching system.
;;
;; * * *
;;
;;     Copyright 2012, Vincent Toups
;;     This program is distributed under the terms of the GNU Lesser 
;;     General Public License (see license.txt).
;;
;; [shadchen-el]:https://github.com/VincentToups/emacs-utils/blob/master/shadchen.el
;;

(require 'cl)

(defun shadchen:unique-symbols (lst)
  (let ((seen (list)))
    (loop for element in lst do 
         (when (not (memq element seen))
           (push element seen)))
    (reverse seen)))
(defstruct match-fail-struct)

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

(defvar *match-fail* (make-match-fail-struct))
(lexical-let ((match-fail *match-fail*)) 
  (defun match-fail-p (o)
    "T only for the match fail singleton."
    (eq match-fail o)))
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

(defun shadchen:replace-colons (o with)
  (cond ((eq o ':) with)
        ((listp o) (mapcar (lambda (o)
                             (shadchen:replace-colons o with)) o))
        (:otherwise o)))

(defun shadchen:pprint-to-string (form)
  "Pretty print form to a string."
  (let ((sym (gensym)))
    (replace-regexp-in-string (regexp-quote (format "%s" sym))
                              ":" 
                              (with-temp-buffer 
                                (cl-prettyprint (shadchen:replace-colons form sym))
                                (buffer-substring (point-min) (point-max))))))

(defun non-keyword-symbol (o)
  (and (symbolp o)
       (not (keywordp o))))

(defun match-list-expander* (sub-expressions match-value body)
  (cond 
    ((not sub-expressions) `(if (not ,match-value) (progn ,@body) *match-fail*))
    ((and (= 1 (length sub-expressions))
          (listp (car sub-expressions))
          (eq (car (car sub-expressions)) 'tail))
     `(match1 ,(cadr (car sub-expressions)) ,match-value ,@body))
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

(defun match-maybe-funcall-expander (match-expression match-value body)
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
       (if (eq ,result-name *match-fail*)
           *match-fail* 
           (match1 ,(caddr match-expression) ,result-name ,@body)))))

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
  (let ((stub-args (gensym "args")))
    `(setf (gethash ',name *extended-patterns*)
           #'(lambda (&rest ,stub-args)
               (destructuring-bind ,args ,stub-args ,@body)))))

(defun match-literal-string (match-expression match-value body)
  `(if (equalp ,match-expression ,match-value) 
       (progn ,@body)
       *match-fail*))

(defun match-literal-number (match-expression match-value body)
  `(if (equalp ,match-expression ,match-value)
       (progn ,@body)
       *match-fail*))

(defun match-literal-keyword (match-expression match-value body)
  `(if (equalp ,match-expression ,match-value)
       (progn ,@body)
       *match-fail*))

(defun match-let-expander (match-expression match-value body)
  `(,(adjust-let-for-mode 'let) ,(cdr match-expression) ,@body))

;; (defun match-or-expander (match-expression match-value body)
;;   (cond 
;;    ((length=1 (cdr match-expression))
;;     `(match1 ,(cadr match-expression) ,match-value ,@body))
;;    (:otherwise
;; 	(let* ((forms (cdr match-expression))
;; 		   (form (car forms))
;; 		   (rest (cdr forms))
;; 		   (nm (gensym "MATCH-OR-EXPANDER-NM-")))
;; 	  `(let* ((,nm ,match-value)
;; 			  (result (match1 ,form ,nm ,@body)))
;; 		 (if (not (eq *match-fail* result))
;; 			 result
;; 		   (match1 (or ,@rest) ,nm ,@body)))))))

(defun match-or-expander-unsafe (match-expression match-value body)
  (let ((-result-holder- (gensym "result-holder-"))
        (-value-holder- (gensym "value-holder-")))
    `(let ((,-value-holder- ,match-value)
           (,-result-holder- *match-fail*))
       (cond 
         ,@(loop for pattern in (cdr match-expression) collect
                `((progn 
                    (setq ,-result-holder- 
                          (match1 ,pattern ,-value-holder- ,@body))
                    (not (eq ,-result-holder- *match-fail*)))
                  ,-result-holder-))
         (:else *match-fail*)))))

(defun match-or-expander (match-expression match-value body)
  (assert (apply #'equal-by-binding (cdr match-expression))
          (match-expression)
          "Or sub-expressions %S contains sub-forms which do not bind identical sets of symbols.")
  (match-or-expander-unsafe match-expression match-value body))

(defun shadchen:mapcat (f lst)
  "Concatenate the results of applying f to each element in lst."
  (loop for item in lst append (funcall f item)))

(defun shadchen:mapcat2 (f lst1 lst2)
  "Concatenate the results of applying f to each element in lst."
  (loop for item1 in lst1
     and item2 in lst2 
     append (funcall f item1 item2)))

(defun calc-pattern-bindings-extended (expr)
  "Calculate the bound symbols of a user defined pattern."
  (let* ((pattern-args (cdr expr))
         (pattern-fun (gethash (car expr) *extended-patterns*))
         (expansion (apply pattern-fun pattern-args)))
    (calc-pattern-bindings expansion)))

(defun calc-backquote-bindings (expr)
  "Calculate the bindings for a backquote expression."
  (loop for sub in (cdr expr) 
     when (and (listp sub)
               (eq (car sub) 'uq))
     append 
       (calc-pattern-bindings (cadr sub))))

(defun calc-pattern-bindings-list (expr &optional acc)
  (cond ((null expr)
         acc)
        ((and (listp expr)
              (listp (car expr))
              (eq 'tail (car (car expr))))
         (append acc (calc-pattern-bindings (cadr (car expr)))))
        (t
         (calc-pattern-bindings-list (cdr expr)
                                     (append (calc-pattern-bindings (car expr)) acc)))))

(defun calc-pattern-bindings (expr)
  "Given a shadchen pattern EXPR return a list of symbols bound
by that expression."
  (cond 
    ((non-keyword-symbol expr)
     (list expr))
    ((vectorp expr)
     (calc-pattern-bindings `(list ,@(coerce expr 'list))))
    ((or (not expr)
         (symbolp expr)
         (numberp expr)
         (stringp expr)) nil)
    ((extended-patternp (car expr))
     (calc-pattern-bindings-extended expr))
    ((listp expr)
     (case (car expr)
       (quote nil)
       ((and values) 
        (shadchen:mapcat #'calc-pattern-bindings (cdr expr)))
       (list (calc-pattern-bindings-list (cdr expr)))
       (cons (append (calc-pattern-bindings (car expr))
                     (calc-pattern-bindings (cdr expr))))
       ((? p funcall maybe-funcall) (if (= 2 (length expr)) nil
                                        (calc-pattern-bindings (elt expr 2))))
       (or (calc-pattern-bindings (cadr expr)))
       (bq (calc-backquote-bindings expr))
       ((! must-match string number keyword non-keyword-symbol) (calc-pattern-bindings (cadr expr)))
       (one-of (calc-pattern-bindings (cadr expr)))
       (let (mapcar #'car (cdr expr)))))
    (:otherwise 
     (error "calc-pattern-bindings: unrecognized pattern %S." expr))))

(defun symbol->string-for-sort (s)
  (symbol-name s))

(defun canonical-binding-list (l)
  (sort* (shadchen:unique-symbols l) #'string< :key #'symbol->string-for-sort))

(defun equal-by-binding2 (p1 p2)
  (equal (canonical-binding-list 
          (calc-pattern-bindings p1))
         (canonical-binding-list 
          (calc-pattern-bindings p2))))

(defun equal-by-binding (&rest patterns)
  (cond 
    ((= 1 (length patterns)) t)
    ((= 2 (length patterns))
     (equal-by-binding2 (car patterns) (cadr patterns)))
    (t
     (and (equal-by-binding2 (car patterns) (cadr patterns))
          (apply #'equal-by-binding (cdr patterns))))))

(defun match-one-of-expander (match-expr value-expr body)
  (let ((-result- (gensym "result-"))
        (-value- (gensym "value-"))
        (-element- (gensym "element-")))
    `(let ((,-result- *match-fail*)
           (,-value- ,value-expr))
       (when (listp ,-value-)
         (loop for ,-element- in ,-value- 
            do 
              (setq ,-result-
                    (match1 ,(cadr match-expr) ,-element- ,@body))
            until 
              (not (eq *match-fail* ,-result-))))
       ,-result-)))

(defun match-vector-expander (match-expr match-val body)
  (let ((value (gensym)))
    `(let ((,value ,match-val))
       (if (vectorp ,value)
           (match1 (list ,@(coerce match-expr 'list))
                   (coerce ,value 'list)
                   ,@body)
           *match-fail*))))

(defun must-match-case (match-expr)
  (cond 
    ((and (listp match-expr)
          (= 2 (length match-expr)))
     :pattern-only)
    ((and (listp match-expr)
          (= 4 (length match-expr)))
     :pattern+)
    (t :unrecognized)))

(defun match-must-match-expander (match-expr val-expr body)
  (case (must-match-case match-expr)
    (:pattern-only 
     (destructuring-bind (_ pattern) match-expr
       (let ((sym (gensym))) 
         (match-must-match-expander 
          `(must-match 
            ,pattern 
            ,sym 
            (format ,(format "must-match pattern (%S) failed to match %%S" pattern) 
                    ,sym))
          val-expr body))))
    (:pattern+
     (destructuring-bind (_ pattern fail-pattern message-expression) match-expr
       (let ((bound-symbols (calc-pattern-bindings pattern))
             (value (gensym))
             (result (gensym)))
         `(match1 (funcall 
                   (lambda (,value)
                     (let ((,result (match1 ,pattern ,value
                                            (list ,@bound-symbols))))
                       (if (eq *match-fail* ,result)
                           (match ,value
                                  (,fail-pattern (let ((,value ,message-expression))
                                                   (if (stringp ,value)
                                                       (error ,value)
                                                       (error "%S" ,value))))
                                  (,(gensym)
                                    (error 
                                     (format 
                                      ,(format "must-match pattern (%S) failed and then the failed-value pattern (%S) also failed on value %%S" 
                                               pattern fail-pattern) 
                                      ,value))))
                           ,result)))
                   (list ,@bound-symbols))
                  ,val-expr
                  ,@body))))
    (t (error "Unrecognized must-match pattern form %S" match-expr))))

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
    ((vectorp match-expression)
     (match-vector-expander match-expression match-value body))
    ((keywordp match-expression)
     (match-literal-keyword match-expression match-value body))
    ((extended-patternp (car match-expression)) 
     (match-extended-pattern-expander match-expression match-value body))
    ((listp match-expression)
     (if match-expression 
         (case (car match-expression)
           ((! must-match) (match-must-match-expander match-expression match-value body))
           (list (match-list-expander match-expression match-value body))
           (cons (match-cons-expander match-expression match-value body))
           (quote (match-quote-expander match-expression match-value body))
           (and (match-and-expander match-expression match-value body))
           ((? p) (match-?-expander match-expression match-value body))
           (funcall (match-funcall-expander match-expression match-value body))
           (maybe-funcall (match-maybe-funcall-expander match-expression match-value body))
           (or (match-or-expander match-expression match-value body))
           (bq (match-backquote-expander match-expression match-value body))
           (let (match-let-expander match-expression match-value body))
           (values (match-values-expander match-expression match-value body))
           (one-of (match-one-of-expander match-expression match-value body))
           (otherwise "MATCH1: Unrecognized match expression: %s." match-expression))
         (match-list-expander '(list) match-value body)))
    (:otherwise (error "MATCH1: Unrecognized match expression: %s." match-expression))))

(defmacro* match-helper (current-match-form value &body forms)
  (assert (symbolp value)
          (value)
          "MATCH-HELPER: VALUE must be a symbol!  Got %s." value)
  (cond 
    ((not forms) `(error "No Match for %s in %S!" ,value ',current-match-form))
    ((listp forms)
     (let ((first-form (car forms)))
       (assert (and (listp first-form)
                    (> (length first-form) 1))
               (first-form current-match-form)
               "Each MATCH SUB-FORM must be at least two elements long, a matcher
and an expression to evaluate on match. Got %S instead (in %S)." 
               first-form current-match-form)
       (let ((match-expression (car first-form))
             (match-body-exprs (cdr first-form))
             (result-name (gensym "MATCH-HELPER-RESULT-NAME-")))
         `(let ((,result-name 
                 (match1 ,match-expression ,value ,@match-body-exprs)))
            (if (not (eq *match-fail* ,result-name)) ,result-name
                (match-helper ,current-match-form ,value ,@(cdr forms)))))))))

(defvar current-match-form :no-form)
(defmacro* match (value &body forms)
  "Attempt to match VALUE against each of the patterns in the CAR of
FORMS.  When a match is detected, its subsequent forms are executed as
in a PROGN where the bindings implied by the match are in effect.  

An error is thrown when no matches are found."
  (declare (debug (form &rest sexp))(indent 1))
  (let ((name (gensym "MATCH-VALUE-NAME-"))
        (current-match-form `(match ,value ,@forms)))
    `(let ((,name ,value)) 
       (match-helper ,current-match-form ,name ,@forms))))

(defmacro* lexical-match (value &body forms)
  "Attempt to match VALUE against each of the patterns in the CAR of
FORMS.  When a match is detected, its subsequent forms are executed as
in a PROGN where the bindings implied by the match are in effect.  

An error is thrown when no matches are found.  Bindings are
lexical via cl.el's lexical let.  An alternative is to use Emacs
24's lexical binding mode and use regular match."
  (declare (debug (form &rest sexp)))
  (let ((*shadchen-binding-mode* :lexical))
    (macroexpand-all `(match ,value ,@forms))))

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
  (let ((n (length patterns)))
    (cond ((= n 0) '(list))
          ((= n 1) `(list (tail ,(car patterns))))
          (:otherwise
           `(list ,@(reverse (cdr (reverse (copy-list patterns))))
                  (tail ,(car (reverse patterns))))))))

(defpattern list* (&rest pats)
  `(list-rest ,@pats))

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

;; (defpattern one-of (pattern)
;;   `(and
;; 	(? #'listp)
;; 	(funcall #'length (? (lambda (x) (> x 0))))
;; 	(or (funcall #'car ,pattern)
;; 		(funcall #'cdr (one-of ,pattern)))))

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
results can occur if `recur` is used in another position.s"
  (declare (debug (sexp &rest form))(indent 1))
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

(defmacro* match-let* (binders &body body)
  "Like let* except patterns may appear at any location a binding symbol appears."
  (declare (indent 1))
  (cond ((null binders)
         `(progn ,@body))
        (t
         (let* ((first (car binders))
                (pattern (car first))
                (value-expr (cadr first))
                (rest (cdr binders))
                (anything-else (gensym)))
           `(match ,value-expr
                   (,pattern (match-let* ,rest ,@body))
                   (,anything-else 
                    (format "In match-let* %S failed to match pattern %S." ,anything-else ',pattern)))))))

(defmacro* lexical-match-let ((&rest binders) &body body)
  "Like let but the left-hand-side of each binder pair can be a
shadchen-pattern.  Within a match-let body, the phrase `(recur
arg1 ...)  can be used to trigger a trampolined re-entry into the
match, but only in tail position.  

At the moment, this is not checked at compile time, so unexpected
results can occur if `recur` is used in another position.

Bindings are lexical via cl.el's lexical-let.  An alternative is
to use Emacs 24 & >'s lexical binding mode with regular match-let."
  (declare (indent 1))
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
(defvar *match-function-name-table* (make-hash-table))
(defvar *match-defun-compile-debug-messages* nil)

(defun match-fboundp (symbol)
  "Returns T when symbol is a function and a match function."
  (and (fboundp symbol)
       (gethash symbol *match-function-table*)))

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
          finally (return ,results)))))

(defun make-defun-match-unbound (name)
  "Make the match function represented by the symbol NAME unbound."
  (puthash name nil *match-function-doc-table*)
  (puthash name nil *match-function-table*)
  (let ((names (gethash name *match-function-name-table*)))
    (loop for f in names do 
         (fmakunbound f)))
  (fmakunbound name))

(defvar *shadchen-recur-sigils* (make-hash-table) "Ensure
different bodies of the same shadchen pattern function get the
same recursion markers.")

(defun get-recur-sigil-for (function-name)
  "Fetch the recursion sigil for the match function function-name."
  (let ((s (gethash function-name *shadchen-recur-sigils*)))
    (if s s
        (progn
          (puthash 
           function-name 
           (gensym (format "defun-match-recur-sigil-for-%s-" function-name))
           *shadchen-recur-sigils*)
          (get-recur-sigil-for function-name)))))

;; (defmacro* defun-match- (name pattern &body body)
;;   "Identical to defun-match- except makes any previous
;; defun-match definitions unbound before defun'ing the function."
;;   `(progn 
;; 	 (make-defun-match-unbound ',name)
;; 	 (defun-match ,name ,pattern ,@body)))

(defun shadchen:get-doc-from-body (body)
  (if (stringp (car body)) (car body)
      ""))

(defun shadchen:get-body-from-body (body)
  body)

(defun shadchen:make-single-pattern-name (name pattern)
  (intern (replace-regexp-in-string 
           (rx (or ")" "(" " "))
           (lambda (m)
             (cond ((string= m "(") "(")
                   ((string= m ")") ")")
                   ((string= m " ") " ")))
           (format "%s-%S" name pattern))))

(defun make-match-defun-main-lambda (name patterns)
  (let ((f-name (gensym "match-sub-fun-"))
        (result (gensym "match-result-"))
        (entry-args (gensym (concat (symbol-name name) "-entry-point-arguments-")))
        (detect-recur (gensym (concat (symbol-name name) "-recur-detector-")))
        (funs-to-test (gensym "funs-to-test-"))
        (funs-left-to-test (gensym "funs-left-to-test-")))
    `(lambda (&rest ,entry-args)
       (let ((,result *match-fail*)
             (,funs-to-test (gethash ',name *match-function-table*))
             (,funs-left-to-test (gethash ',name *match-function-table*))
             (,f-name nil))
         (flet ((recur (&rest args)
                  (cons ',(get-recur-sigil-for name) args))
                (,detect-recur (o)
                  (and (consp o)
                       (let ((test-result (eq (car o) ',(get-recur-sigil-for name))))
                         test-result))))
           ,@(if *match-defun-compile-debug-messages*
                 `((message "Entering %s." ',name))
                 nil)
           (loop while ,funs-left-to-test
              do
                (setq ,f-name (pop ,funs-left-to-test))
                ,@(if *match-defun-compile-debug-messages*
                      `((message "Trying %s against %S." ,f-name ,entry-args))
                      nil)
                (setq ,result (apply ,f-name ,entry-args))
              when (and (not (match-fail-p ,result))
                        (not (,detect-recur ,result)))
              do
                ,@(if *match-defun-compile-debug-messages*
                      `((message "Terminating with %s." ,result))
                      nil)
                (return ,result)
              when (,detect-recur ,result)
              do
                ,@(if *match-defun-compile-debug-messages*
                      `((message "Recurring to %s with %s." ',name ,result))
                      nil)
                (setq ,entry-args (cdr ,result))
                (setq ,funs-left-to-test ,funs-to-test))
           (if (match-fail-p ,result)
               (error "%s: Match failure for arguments: %S." ',name ,entry-args)
               ,result))))))

(defmacro* defun-match- (name patterns &body body)
  (let* ((doc-string (shadchen:get-doc-from-body body))
         (real-body (shadchen:get-body-from-body body))
         (compound-name (shadchen:make-single-pattern-name name patterns))
         (args (gensym (concat "args-" (symbol-name compound-name))))
         (extended-doc-string (concat (format "%S" patterns) "- " doc-string (format "\n"))))
    `(progn 
       (defun ,compound-name (&rest args)
         ,extended-doc-string 
         (match1 (list ,@patterns) args  ,@body))
       (setf (gethash ',name *match-function-table*)
             (list #',compound-name))
       (setf (gethash ',name *match-function-doc-table*)
             ,extended-doc-string)
       (setf (gethash ',name *match-function-name-table*)
             (list ',compound-name))
       (defalias ',name ,(make-match-defun-main-lambda name patterns) 
         ,extended-doc-string))))

(defmacro* defun-match (name patterns &body body)
  (let* ((doc-string (shadchen:get-doc-from-body body))
         (real-body (shadchen:get-body-from-body body))
         (compound-name (shadchen:make-single-pattern-name name patterns))
         (args (gensym (concat "args-" (symbol-name compound-name))))
         (extended-doc-string (concat (format "%S" patterns) "- " doc-string (format "\n"))))
    `(progn 
       (defun ,compound-name (&rest args)
         ,extended-doc-string 
         (match1 (list ,@patterns) args  ,@body))
       (setf (gethash ',name *match-function-table*)
             (append (gethash ',name *match-function-table*) 
                     (list #',compound-name)))
       (setf (gethash ',name *match-function-doc-table*)
             (concat (gethash ',name *match-function-doc-table*)
                     (format "\n")
                     ,extended-doc-string))
       (setf (gethash ',name *match-function-name-table*)
             (cons ',compound-name (gethash ',name *match-function-name-table*)))
       (defalias ',name ,(make-match-defun-main-lambda name patterns) 
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
             (or 
              (funcall 
               (lambda (s)
                 (substring s ,pivot))
               (concat ,@(cdr patterns)))
              (full-concat ,(+ pivot 1) ,@patterns)))
        (full-concat ,(+ pivot 1) ,@patterns))))))



(defpattern append-helper (head-pattern tail-patterns)
  `(or 
    (cons ,head-pattern (append ,@tail-patterns))
    (and 
     (p #'(lambda (l) (cdr l)))
     (funcall 
      (lambda (p)
        (let ((candidate (car p))
              (rest (cdr p)))
          (cons (reverse (cons (car rest) (reverse candidate)))
                (cdr rest))))
      (append-helper ,head-pattern ,tail-patterns)))))

(defpattern append (&rest patterns)
  (cond 
    ((length=0 patterns) nil)
    ((length=1 patterns)
     `(and (p #'listp ,(car patterns))))
    (:otherwise
     (let ((hd (car patterns))
           (rest (cdr patterns)))
       `(funcall (lambda (l) 
                   (cons nil l)) (append-helper ,hd ,rest))))))

(defmacro* match-lambda (&body body)
  "Produce a lambda accepting a single argument and exectuting
the matching expression from the body."
  (let ((arg (gensym "arg-")))
    `(lambda (,arg)
       (match ,arg 
              ,@body))))

(defmacro* shadchen:sequentially (bindings &body body)
  "List comprehension monad form."
  (cond ((null bindings)
         `(progn ,@body))
        (:else
         (let* ((binding (car bindings))
                (sym (car binding))
                (expr (cadr binding)))
           `(loop for ,sym in ,expr append 
                 (shadchen:sequentially ,(cdr bindings) ,@body))))))

(defun* shadchen:enumerate-segments (len n &optional (offset 0) acc) 
  (cond 
    ((= n 0) (reverse acc))
    ((= n 1) (reverse (cons (list (list offset (- len 1))) acc)))
    ((= n 2) (loop for i from offset to len collect
                  (append acc (list (list offset i)
                                    (list i len)) )))
    (:else
     (loop for i from offset to len append
          (progn 
            (shadchen:enumerate-segments len (- n 1) i 
                                         (cons (list offset i) acc)))))))

(defun shadchen:substrings (string delims)
  (mapcar (lambda (delim)
            (substring string (car delim) (cadr delim)))
          delims))

(defun shadchen:enumerate-substrings (string n-sub)
  (let ((segments (shadchen:enumerate-segments (length string) n-sub)))
    (reverse (mapcar
              (lambda (segment)
                (shadchen:substrings string segment))
              segments))))

;; (defpattern concat (&rest patterns)
;;   (cond 
;;    ((length=0 patterns)
;; 	"")
;;    ((length=1 patterns)
;; 	`(? #'stringp ,(car patterns)))
;;    (:otherwise
;; 	(cond 
;; 	 ((stringp (car patterns))
;; 	  `(simple-concat ,@patterns))
;; 	 (:otherwise 
;; 	  `(full-concat 0 ,@patterns))))))

(defpattern concat (&rest patterns)
  (let ((-string- (gensym "-string-")))
    `(and (p #'stringp)
          (funcall
           (lambda (,-string-)
             (shadchen:enumerate-substrings ,-string- ,(length patterns)))
           (one-of (list ,@patterns))))))

(defun shadchen:non-keyword-symbolp (o)
  (and (symbolp o)
       (not (keywordp o))))

(defpattern keyword (pattern) 
  `(p #'keywordp ,pattern))

(defpattern symbol (pattern) 
  `(p #'symbolp ,pattern))

(defpattern non-kw-symbol (pattern)
  `(p #'shadchen:non-keyword-symbolp ,pattern))

(defpattern string (pattern)
  `(p #'stringp ,pattern))

(defpattern number (pattern)
  `(p #'numberp ,pattern))

(defpattern equal (value &optional pattern)
  (let ((arg (gensym "arg")))
    (if pattern `(p #'(lambda (,arg) (equal ,arg ,value)) ,pattern)
        `(p #'(lambda (,arg) (equal ,arg ,value))))))

(defpattern not-equal (value &optional pattern)
  (let ((arg (gensym "arg")))
    (if pattern `(p #'(lambda (,arg) (not (equal ,arg ,value))) ,pattern)
        `(p #'(lambda (,arg) (not (equal ,arg ,value)))))))

(defpattern non-null (&optional (pattern (gensym)))
  (let ((arg (gensym)))
    `(p (lambda (,arg)
          (not (null ,arg)))
        ,pattern)))

(defpattern cons* (car-pattern cdr-pattern)
  `(p #'consp (cons ,car-pattern ,cdr-pattern)))

(defstruct monad bind return plus zero)
(defun copy-monad (m)
  (make-monad 
   :bind (monad-bind m)
   :return (monad-return m)
   :plus (monad-plus m)
   :zero (monad-zero m)))
(defvar monad:id 
  (make-monad :bind (lambda (x f) (funcall f x))
			  :return (lambda (x) x)))
(defvar *current-monad* monad:id)

(defun current-monad-bind (mv mf)
  (funcall (monad-bind *current-monad*) mv mf))
(defun current-monad-return (v)
  (funcall (monad-return *current-monad*) v))
;;(defun -> (v)
;;  (current-monad-return v))
(defun current-monad-plus (mv1 mv2)
  (funcall (monad-plus *current-monad*)
		   mv1 mv2))
(defvar current-monad-zero nil)

(defmacro flet-aliases (binders expressions)
  (let ((gensyms (mapcar (lambda (_) (gensym)) binders))
		(exprs (mapcar (match-lambda 
						((list _ expr) expr)) binders))
		(names (mapcar (match-lambda
						((list name _) name)) binders))
		(lets (mapcar* #'list gensyms exprs))
		(flets 
		 (mapcar* 
		  (lambda (name gen-name)
			(let ((args (gensym)))
			  `(,name (&rest ,args)
					  (apply ,gen-name ,args)))))))
	`(let ,lets
	   (flet ,flets ,@expressions))))

(defmacro labels-aliases (binders expressions)
  (let ((gensyms (mapcar (lambda (_) (gensym)) binders))
		(exprs (mapcar (match-lambda 
						((list _ expr) expr)) binders))
		(names (mapcar (match-lambda
						((list name _) name)) binders))
		(lets (mapcar* #'list gensyms exprs))
		(flets 
		 (mapcar* 
		  (lambda (name gen-name)
			(let ((args (gensym)))
			  `(,name (&rest ,args)
					  (apply ,gen-name ,args)))))))
	`(let ,lets
	   (labels ,flets ,@expressions))))

(eval-when (load compile eval) 
  (defvar shadchen:absent 'e5671794bf87ebab2a0d5e0ded530e68)
  (defun shadchen:absent (x)
	(eq x shadchen:absent))
  (defun shadchen:not-absent (x)
	(not (shadchen:absent x)))
  (defun-match- shadchen:all-absent (nil)
	t)
  (defun-match shadchen:all-absent ((list hd (tail tl)))
	(if (shadchen:absent hd) 
		(recur tl)
	  nil))
  (defpattern shadchen:monad-bind (pattern expr)
	`(or (list :bind ,pattern ,expr)
		 (list ,pattern '<- ,expr)))
  (defpattern shadchen:monad-let (pattern expr)
	`(or (list :let ,pattern ,expr)
		 (list ,pattern '= ,expr))))

(defmacro* monadically-helper (&body expressions)
  (match expressions
		 ((list (shadchen:monad-bind pattern expr) (tail subsequents))
		  (let ((val (gensym)))
			`(current-monad-bind 
			  ,expr
			  (lambda (,val)
				(match ,val
					   (,pattern ,(if subsequents `(monadically-helper ,@subsequents) val))
					   (,val (error "monadically pattern <%s> failed to match %s during monadic-bind." ',pattern ,val)))))))
		 ((list (shadchen:monad-let pattern expr) (tail subsequents))
		  (let ((val (gensym)))
			`(match ,expr
					((and ,val ,pattern) ,(if subsequents `(monadically-helper ,@subsequents) val))
					(,val (error "monadically pattern <%s> failed to match %s during non-monadic bind." ',pattern ,val)))))
		 ((list (list :aside (tail expressions)) rest0 (tail rest))
		  `(progn ,@expressions
				  (monadically-helper ,rest0 ,@rest)))
		 ((list expr subsequents0 (tail subsequents))
		  (let ((val (gensym)))
			`(current-monad-bind ,expr
								 (lambda (,val)
								   (monadically-helper subsequents0 ,@subsequents)))))
		 ((list expr)
		  expr)))




(defmacro* monadically ((&key 
						 (monad shadchen:absent) 
						 (bind shadchen:absent) 
						 (return shadchen:absent) 
						 (plus shadchen:absent) 
						 (zero shadchen:absent)) 
						&body body)
  (cond 
   ((shadchen:all-absent (list monad bind return plus zero))
	`(symbol-macrolet ((current-monad-zero '(monad-zero *current-monad*))) 
	   (monadically-helper ,@body)))
   (:otherwise 
	(let ((-monad (gensym)))
	  `(let ((,-monad (copy-monad ,(if (shadchen:absent monad) `*current-monad*
									 monad))))
		 ,@(if (shadchen:absent bind) nil (list `(setf (monad-bind ,-monad) ,bind)))
		 ,@(if (shadchen:absent return) nil (list `(setf (monad-return ,-monad) ,return)))
		 ,@(if (shadchen:absent plus) nil (list `(setf (monad-plus ,-monad) ,plus)))
		 ,@(if (shadchen:absent zero) nil (list `(setf (monad-zero ,-monad) ,zero)))
		 (let ((*current-monad* ,-monad))
		   (monadically-helper ,@body)))))))

(defvar list-monad
  (make-monad :bind 
			  (lambda (mv mf)
				(shadchen:mapcat mf mv))
			  :return #'list
			  :plus #'append
			  :zero nil))

(defun Just (x)
  `(Just ,x))
(defun None (&optional reasons)
  `(None ,reasons))

(eval-when (load compile eval)
  (defpattern Just (&optional (pattern (gensym)))
	`(list 'Just ,pattern))
  (defpattern None (&optional (pattern (gensym)))
	`(list 'None ,pattern)))

(defvar maybe-monad 
  (make-monad :bind (lambda (mv mf)
					  (match mv 
							 ((Just value)
							  (funcall mf value))
							 ((None reason)
							  mv)))
			  :return (lambda (v) (Just v))
			  :plus (lambda (mv1 mv2)
					  (match mv1
							 ((Just value)
							  mv2)
							 ((None reason)
							  mv1)))
			  :zero (None)))

(defun shadchen:kw->symbol (k)
  (assert (keywordp k))
  (intern (substring (symbol-name k) 1)))

(eval-when (compile load eval) (defpattern keys (&rest kp-pairs)
  (let ((db-expr '(&key))
		(bound-symbols nil)
		(patterns nil)
		(datum (gensym "datum"))) 
	(loop for (k p . rest) on kp-pairs by #'cddr
		  do
		  (let ((temp (gensym "temp"))
				(key-as-sym (shadchen:kw->symbol k)))
			(setq bound-symbols (append bound-symbols (list key-as-sym)))
			(setq db-expr (append db-expr (list key-as-sym)))
			(setq patterns (append patterns (list p)))))
	`(maybe-funcall 
	  (lambda (,datum)
		(condition-case nil 
			(destructuring-bind ,db-expr ,datum
			  (list ,@bound-symbols))
		  (error *match-fail*)))
	  (list ,@patterns)))))


;; A few obvious EmacsLisp extras

(defun shadchen/extract (key type)
  "Return a func to extract KEY from TYPE. 

TYPE is either `:alist' or `:plist'."
  (lexical-let ((typ type)
                (k key))
    (lambda (kvlist)
      (case typ
        (:struct (apply (symbol-function k) (list kvlist)))
        (:plist (plist-get kvlist k))
        (:alist (cdr-safe (assoc k kvlist)))))))

(defpattern plist (&rest kv-pairs) 
  `(and ,@(loop for (k v . rest) on kv-pairs by #'cddr
             collect
               `(funcall (shadchen/extract ,k :plist) ,v))))

(defpattern alist (&rest kv-pairs)
  `(and ,@(loop for (k v . rest) on kv-pairs by #'cddr
             collect
               `(funcall (shadchen/extract ,k :alist) ,v))))


(defun shadchen/struct-field (struct field)
  "Helper to access FIELD in STRUCT."
  (concat (symbol-name struct) "-" (symbol-name field)))

(defpattern struct (name &rest accessor-pairs)
  `(and (? (lambda (v)
             (eq (aref v 0)
                 (intern (concat "cl-struct-" ,(symbol-name name))))) _)
        ,@(loop for (k v . rest) on accessor-pairs by #'cddr
             collect
               `(funcall (shadchen/extract
                          ,(shadchen/struct-field name k) :struct)
                         ,v))))

(provide 'shadchen)

;;; shadchen.el ends here
