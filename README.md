Shadchen: A pattern matching library
====================================

    shadchen: Noun
      matchmaker
    from Yiddish

(note: there is an emacs lisp port of this library
 [here][shadchen-el])
(note: if you are reading this README for the emacs version of the
 library, keep in mind that emacs symbols are case sensitive.  Symbols
 are all lowercase in this library.)


I love pattern-matching, which I find to be a great way to combine
destructuring data with type-checking when used in dynamic languages.
If you aren't familiar with how pattern matching works, here is an
example:

    (defun second (lst)
     (match lst 
      ((cons _ (cons x rest)) x)))

`MATCH` introduces a pattern matching expression, which takes a value,
in this case `LST` and a series of lists, whose first elements are
descriptions of a data structure and whose subsequent elements are
code to execute if the match succeeds.  Pattern matching takes the
description of the data and binds the variables that appear therein to
the parts of the data structure they indicate.  Above, we match `_` to
the `car` of a list, `x` to the `car` of that list's `cdr`, and `rest`
to the `cdr` of that list.  

If we don't pass in a list, the match fails.  (Because of the behavior
of CL's `car` and `cdr`, which return `NIL` on `NIL`, the form `cons`
doesn't enforce a length requirement on the input list, and will
return `NIL` for an empty list.  This corresponds with the fact that
in Common Lisp `(car nil)` is `nil` and `(cdr nil)` is `nil`.)

We might instead write:

    (defun second-of-two (lst)
      (match lst
        ((list _ x) x)))

Which returns the second element of a list _only_ when a two element
list is passed in.  `MATCH` can take multiple pattern/body sets, in
which case patterns are tried in order until one pattern matches, and
the result of evaluating the associated forms is returned.  If no
patterns match, an error is raised.

Built-in Patterns
-----------------

Shadchen supports the following built-in patterns.

    <SYMBOL>

Matches anything, binding <SYMBOL> to that value in the body
expressions.

    <KEYwORD-LITERAL> 

Matches only when the value is the same keyword.

    <NUMBER-LITERAL>

Matches only when the value is the same number.

    <STRING-LITERAL>

Matches only when the value is `string=` is the same string.	

    (CONS <PATTERN1> <PATTERN2>)

Matches any `CONS` cell, or `NIL`, then matches `<PATTERN1>` and
`<PATTERN2>`, executing the body in a context where their matches are
bound.  If the match value is NIL, then each `PATTERN` matches against
NIL.

    (LIST <P1> ... <PN>)

Matches a list of length N, then matches each pattern `<PN>` to the
elements of that list.

    (LIST-REST <P1> ... <PN> <REST-PATTERN)

Matches <P1> - <PN> to elements in at list, as in the `LIST` pattern.
The final `<REST-PATTERN>` is matched against the rest of the list.

    (QUOTE DATUM)

Only succeeds when `DATUM` is `EQUALP` to the match-value.  Binds no
values.

     (AND <P1> .. <PN>)

Tests all `<PN>` against the same value, succeeding only when all
patterns match, and binding all variables in all patterns.

     (OR <P1> .. <PN>)

Tries each `<PN>` in turn, and succeeds if any `<PN>` succeeds.  The
body of the matched expression is then executed with that `<PN>'s`
bindings.  It is up to the user to ensure that the bindings are
relevant to the body.

     (? PREDICATE <PATTERN>)

Succeeds when `(FUNCALL PREDICATE MATCH-VALUE)` is true and when
`<PATTERN>` matches the value.  Body has the bindings of `<PATTERN>`.  

     (FUNCALL FUN <PATTERN>)

Applies `FUN` to the match value, then matches `<PATTERN>` against _the
result_.

     (BQ EXPR)

Matches as if by `BACKQUOTE`.  If `EXPR` is an atom, then this is
equivalent to `QUOTE`.  If `EXPR` is a list, each element is matches
as in `QUOTE`, unless it is an `(UQ <PATTERN>)` form, in which case it
is matched as a pattern.  Eg:

    (match (list 1 2 3)
      ((BQ (1 (UQ x) 2)) x)) 

Will succeed, binding `X` to 2.  

    (match (list 10 2 20)
       ((BQ (1 (UQ x) 2)) x))

Will fail, since `10` and `1` don't match.

    (values <P1> ... <PN>)

Will match multiple values produced by a `(values ...)` form.

    (let (n1 v1) (n2 v2) ... (nn vn))

Not a pattern matching pattern, per se.  `let` always succeeds and
produces a context where the bindings are active.  This can be used to
provide default alternatives, as in:

    (defun not-nil (x) x)

    (match (list 1) 
     ((cons hd (or (? #'non-nil tl)
                   (let (tl '(2 3)))))
      (list hd tl)))

Will result in `(1 (2 3))` but 

    (match (list 1 4) 
     ((cons hd (or (? #'non-nil tl)
                   (let (tl '(2 3)))))
      (list hd tl)))

Will produce `(1 (4))`.  Note that a similar functionality can be
provided with `funcall`.


Extending shadchen
------------------

Users can define their own patterns using the `defpattern` form.  For
instance, the behavior of `CONS`, which matches the empty list, may
not be desired.  We can define a match which doesn't have this
behavior as:

    (defun non-nil (x) x)
    (defpattern cons* (car cdr)
     `(? #'non-nil (cons ,car ,cdr)))

A pattern is a function which takes the arguments passed into the
custom pattern, and expands them into a new pattern in the language of
the built-in pattern-matching.  

We can now say:

    (match (cons 10 11)
     ((cons* a b) a)) 

Which will produce 10, but:

    (match nil
     ((cons* a b) a))

Will raise a no-match error.  

Judicious application of the matchers `AND`, `FUNCALL`, and `?` allow
the definition of arbitrary matchers without exposing the guts of the
matching system.

* * *

    Copyright 2012, Vincent Toups
    This program is distributed under the terms of the GNU Lesser 
    General Public License (see license.txt).

[shadchen-el]:https://github.com/VincentToups/emacs-utils/blob/master/shadchen.el

