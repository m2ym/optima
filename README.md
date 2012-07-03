optima - Optimized Pattern Matching Library
===========================================

optima is a very fast pattern matching library
which uses optimizing techniques widely used in a functional
programming world. See the following references for more detail:

* [Optimizing Pattern Matching](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.6.5507) by Fabrice Le Fessant, Luc Maranget
* [The Implementation of Functional Programming Languages](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/) by Simon Peyton Jones

Pattern Language
----------------

A pattern specifier, or a pattern for short unless ambiguous, is an
expression that describes how a value matches the pattern. Pattern
specifiers are defined as follows:

    pattern-specifier ::= constant-pattern
                        | variable-pattern
                        | symbol-macro-pattern
                        | constructor-pattern
                        | derived-pattern
                        | guard-pattern
                        | not-pattern
                        | or-pattern
                        | and-pattern
    
    constant-pattern ::= t | nil
                       | atom-except-symbol
                       | (quote VALUE)
    
    variable-pattern ::= SYMBOL | (variable SYMBOL)

    symbol-macro-pattern ::= (symbol-macrolet SYMBOL)
    
    constructor-pattern ::= (NAME ARG*)

    derived-pattern ::= (NAME PATTERN*)
    
    guard-pattern ::= (when TEST-FORM)
    
    not-pattern ::= (not PATTERN)
    
    or-pattern ::= (or PATTERN*)
    
    and-pattern ::= (and PATTERN*)

### Constant-Pattern

A constant-pattern matches the constant itself.

Examples:

    (match 1 (1 2)) => 2
    (match "foo" ("foo" "bar")) => "bar"
    (match '(1) ('(1) 2)) => 2

### Variable-Pattern

A variable-pattern matches any value and bind the value to the
variable. _ and otherwise is a special variable-pattern (a.k.a
wildcard-pattern) which matches any value but doesn't bind.

Examples:

    (match 1 (x x)) => 1
    (match 1 (_ 2)) => 2
    (match 1
      (2 2)
      (otherwise 'otherwise))
    => OTHERWISE

### Symbol-Macro-Pattern

A symbol-macro-pattern matches any value as variable-patterns but bind
the value with SYMBOL-MACROLET.

Examples:

    (defvar c (cons 1 2))
    (match c ((cons (symbol-macrolet x) y) (incf x) (incf y)))
    c
     => (2 . 2)

### Constructor-Pattern

A constructor-pattern matches not a value itself but a structure of
the value. The following constructors are available:

#### CONS

Syntax:

    cons-constructor-pattern ::= (cons CAR-PATTERN CDR-PATTERN)

Examples:

    (match '(1 . 2)
      ((cons a b) (+ a b)))
     => 3

#### VECTOR

Syntax:

    vector-constructor-pattern ::= (vector PATTERN*)

Examples:

    (match #(1 2)
      ((vector a b) (+ a b)))
    => 3

#### SIMPLE-VECTOR

Syntax:

    simple-vector-constructor-pattern ::= (simple-vector PATTERN*)

Examples:

    (match #(1 2)
      ((simple-vector a b) (+ a b)))
    => 3

#### CLASS

Mathces an instance of any class (of standard-class).

Syntax:

    class-constructor-pattern ::= (class NAME slot*)
                                | (NAME slot*)
    
    slot ::= SLOT-NAME
           | (SLOT-NAME PATTERN*)

CLASS can be omitted. If slot is a symbol, then it will be regarded
as (slot slot). If more than one PATTERN are given, then they will be
wrapped by and-pattern like (and PATTERN*).

Examples:

    (defclass point ()
      ((x :initarg :x)
       (y :initarg :y)))
    (defvar p (make-instance 'point :x 1 :y 2))
    (match p
      ((point x y) (list x y)))
    => (1 2)
    (match p
      ((point (x 1 x) _) x))
    => 1
    (defstruct person (name age))
    (defvar foo (make-person :name "foo" :age 30))
    (match foo
      ((person name age) (list name age)))
    => ("foo" 30)

#### STRUCTURE

Mathces an any value of a structure.

Syntax:

    structure-constructor-pattern ::= (structure CONC-NAME slot*)
                                    | (CONC-NAME slot*)
    
    slot ::= SLOT-NAME
           | (SLOT-NAME PATTERN*)

As well as CLASS constructor-pattern, STRUCTURE can be
omitted. CONC-NAME is a prefix string of a predicate (CONC-NAME +
"p") and accessors (CONC-NAME + SLOT-NAME). For example, if we have
the following defstruct,

    (defstruct person name age)

the structure constructor-pattern (person- name age) is valid because
PERSON-P, PERSON-NAME and PERSON-AGE are available here. Technically,
we don't need a structure defined. If we have the following code, for
instance,

    (defun point-p (p) (consp p))
    (defun point-x (p) (car p))
    (defun point-y (p) (cdr p))

the pattern matching below is valid.

    (match (cons 1 2)
      ((point- x y) (list x y)))
    => (1 2)

Examples:

    (defstruct (person (:conc-name :p-)
                       (:predicate p-p))
      name age)
    (match (make-person :name "foo" :age 30)
      ((p- name age) (list name age)))
    => ("foo" 30)

### Dervied-Pattern

A derived-pattern is a pattern that is defined with DEFPATTERN. There
are some builtin dervied patterns as below:

#### LIST

Expansion of LIST derived patterns=

    (list a b c) => (cons a (cons b (cons c nil)))

#### LIST*

Expansion of LIST* derived patterns:

    (list a b c) => (cons a (cons b c))

#### TYPEP

Expansion of TYPEP derived patterns:

    (TYPEP string) => (and #:G1 (when (typep #:G1 'string)))

#### SATISFIES

Expansion of SATISFIES derived patterns:

    (satisfies evenp) => (and #:G1 (when (evenp #:G1)))

### Guard-Pattern

A guard-pattern is a special pattern that tests TEST-FORM satisfies in
the current matching context. Guard-patterns are basically used with
and-patterns. See the examples below.

Examples:

    (match 1 ((and x (when (evenp x))) 'even))
    => NIL

### Not-Pattern

A not-pattern matches a value that is not matched with sub-PATTERN.

Examples:

    (match 1 ((not 2) 3)) => 3
    (match 1 ((not (not 1)) 1)) => 1

### Or-Pattern

An or-pattern matches a value that is matched with one of
sub-PATTERNs. There is a restriction that every pattern of
sub-PATTERNs must have same set of variables.

Examples:

    (match '(2 . 1) ((or (cons 1 x) (cons 2 x)) x))
    => 1

### And-Pattern

An and-pattern matches a value that is matched with all of
sub-PATTERNs. The most common use case is to match a value and bind
the value to a variable.

Examples:

    (match 1 ((and 1 x) x))
    => 1

[Package] optima
----------------

## [Macro] defpattern

    defpattern name lambda-list &body body

Defines a derived pattern specifier named NAME. This is analogous
to DEFTYPE.

Examples:

    ;; Defines a LIST pattern.
    (defpattern list (&rest args)
      (when args
        `(cons ,(car args) (list ,@(cdr args)))))

## [Macro] match

    match arg &body clauses

Matches ARG with CLAUSES. CLAUSES is a list of the form of (PATTERN
. BODY) where PATTERN is a pattern specifier and BODY is an implicit
progn. If ARG is matched with some PATTERN, then evaluates
corresponding BODY and returns the evaluated value. Otherwise, returns
NIL.

If BODY starts with a symbol WHEN, then the next form will be used to
introduce a guard for PATTERN. That is,

    (match list ((list x) when (oddp x) x))

will be translated to

    (match list ((and (list x) (when (oddp x))) x))

## [Macro] multiple-value-match

    multiple-value-match values-form &body clauses

Matches the multiple values of VALUES-FORM with CLAUSES. Unlike
MATCH, CLAUSES have to have the form of (PATTERNS . BODY), where
PATTERNS is a list of patterns. The number of values that will be used
to match is determined by the maximum arity of PATTERNS among CLAUSES.

Examples:

    (multiple-value-match (values 1 2)
     ((2) 1)
     ((1 y) y))
    => 2

## [Macro] smatch

    smatch arg &body clauses

Same as MATCH, except SMATCH binds variables by SYMBOL-MACROLET
instead of LET. See the documentation of symbol-macro-pattern.

## [Macro] multiple-value-smatch

    multiple-value-smatch values-form &body clauses

Same as MULTIPLE-VALUE-MATCH, except MULTIPLE-VALUE-SMATCH binds
variables by SYMBOL-MACROLET instead of LET.

## [Macro] ematch

    ematch arg &body clauses

Same as MATCH, except MATCH-ERROR will be raised if not matched.

## [Macro] multiple-value-ematch

    multiple-value-ematch values-form &body clauses

Same as MULTIPLE-VALUE-MATCH, except MATCH-ERROR will be raised if
not matched.

## [Macro] esmatch

    esmatch arg &body clauses

Same as EMATCH, except ESMATCH binds variables by SYMBOL-MACROLET
instead of LET.

## [Macro] multiple-value-esmatch

    multiple-value-esmatch values-form &body clauses

Same as MULTIPLE-VALUE-EMATCH, except MULTIPLE-VALUE-ESMATCH binds
variables by SYMBOL-MACROLET instead of LET.

## [Macro] cmatch

    cmatch arg &body clauses

Same as MATCH, except continuable MATCH-ERROR will be raised if not
matched.

## [Macro] multiple-value-cmatch

    multiple-value-cmatch values-form &body clauses

Same as MULTIPLE-VALUE-MATCH, except continuable MATCH-ERROR will
be raised if not matched.

## [Macro] csmatch

    csmatch arg &body clauses

Same as CMATCH, except CSMATCH binds variables by SYMBOL-MACROLET
instead of LET.

## [Macro] multiple-value-csmatch

    multiple-value-csmatch values-form &body clauses

Same as MULTIPLE-VALUE-CMATCH, except MULTIPLE-VALUE-CSMATCH binds
variables by SYMBOL-MACROLET instead of LET.

## [Macro] if-match

    if-match pattern arg &body (then &optional else)

Equivalent to (match ARG (PATTERN THEN) (otherwise ELSE)).

## [Macro] if-smatch

    if-smatch pattern arg &body (then &optional else)

Equivalent to (smatch ARG (PATTERN THEN) (otherwise ELSE)).

## [Macro] when-match

    when-match pattern arg &body body

Equivalent to (match ARG (PATTERN BODY...)).

## [Macro] when-smatch

    when-smatch pattern arg &body body

Equivalent to (smatch ARG (PATTERN BODY...)).

## [Macro] unless-match

    unless-match pattern arg &body body

Equivalent to (match ARG (PATTERN) (otherwise BODY...)).

## [Macro] with-match

    with-match pattern arg &body body

Equivalent to (ematch ARG (PATTERN BODY...)).

## [Macro] lambda-match

    lambda-match &body clauses

Equivalent to (lambda (arg) (match arg CLAUSES...)).

## [Macro] lambda-ematch

    lambda-ematch &body clauses

Equivalent to (lambda (arg) (ematch arg CLAUSES...)).

## [Macro] lambda-cmatch

    lambda-cmatch &body clauses

Equivalent to (lambda (arg) (cmatch arg CLAUSES...)).

[Package] optima.extra
----------------

This package contains derived and constructor patterns with designators not from
COMMON-LISP package.

#### PLIST

Syntax:

    plist-constructor-pattern ::= (plist (key PATTERN)*)

Examples:

    (match '(:one 1 :two 2 :three 3)
      ((plist :one 1 :two x) x))
    => 2

#### ALIST

Syntax:

    alist-constructor-pattern ::= (alist (key . PATTERN)*)

Examples:

    (match '((:one . 1) (:two . 2) (:three . 3))
      ((alist (:one . 1) (:two . x)) x))
    => 2


Authors
-------

* Tomohiro Matsuyama

License
-------

LLGPL
