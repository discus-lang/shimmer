
## Grammar

### Declarations

```
Decl  ::=  '@' Name Param* '=' Exp ';'    (Macro declaration)
```

A declaration consists of a macro name prefixed by ``@``, the macro parameters and
the bound expression. All declarations end with a ``;``.


### Expressions

```
Exp   ::=  Ref                            (External reference)
       |   Key Exp                        (Keyword  application)
       |   Exp Exp+                       (Function application)
       |   Name ('^' Nat)?                (Variable with lifting specifier)
       |   '\' Param+ '.' Exp             (Function abstraction)
       |   Train      '.' Exp             (Substitution train)
```

Expressions consist of references, keyword applications, function applications,
variables, abstractions and substitution trains.

References to external things are are specified separately from variables as there
is no possibility of name capture during substitution.

Keyword applications are used when applying keywords as they only take one argument
expression at a tome.

Function applications consists of a functional expression applied to a list of arguments.

Variables consists of a name and a bump count. If the bump count is zero then it is
elided. The bump count is used to manage name clashes during substitution, for example
in the expression ``(\x. \x. x^1)`` the variable reference ``x^1`` referes to the outer
most binder rather than the inner one, which shares the same name.

Substitutions consist of a substitution train specifying the environment and lifting
information applied to a body expression.


## References

```
Ref   ::= '@' Name                        (Macro reference)
       |  '%' Name                        (Symbol reference)
       |  '#' Name                        (Primitive reference)
       |  '?' Nat                         (Nominal reference)
```

References consist of macro references, symbol references, primitive references,
and nominal references.

Macro references refer to top-level macro declarations. During evaluation macro references
are expanded to their definitions as needed.

Symbol references refer to user defined symbols, which behave like uninterpreted
data constructors.

Primitive references refer to pre-defined primitive names and operators, for example
``#nat-add`` for primitive addition of natural numbers.

Nominal references refer to nominal variables.


## Parameters

```
Param ::= Name                            (Call-by-value parameter)
       |  '!' Name                        (Explicitly call-by-value parameter)
       |  '~' Name                        (Explicitly call-by-name  parameter)
```

Function parameters are either explicitly call-by-value, indicated with a ``!``, or
call-by-name indicated with a ``~``. If not specifier is provided the parameter
is assumed to be call-by-value.


## Keywords

```
Key   ::= '##box'                         (Box an expression, delaying evaluation)
       |  '##run'                         (Run an expression, forcing  evaluation)
```

Keywords are used to control evaluation order.

The ``##box`` keyword is used to suspend evaluation of an expression when it is
used as the argument of a call-by-value abstraction.

The ``##run`` keyword is used to force the evaluation of a boxed expression.


## Substitutions

```
Train ::= Car+                            (Substitution train)

Car   ::= '['  Bind,* ']'                 (Simultaneous substitution)
       |  '[[' Bind,* ']]'                (Recursive substitution)
       |  '{'  Bump,* '}'                 (Lifting specifier)

Bind  ::= Name ('^' Nat)? '=' Exp         (Variable Substitution binding)
       | '?' Nat          '=' Exp         (Nominal  Substitution binding)

Bump  ::= Name ('^' Nat)? ':' Nat         (Lifting bump)
```

Explicit substitutions consists of a train of individual cars.

Substitution cars consist of simultaneous substitutions, recursive substitutions,
and lifting specifiers.

Simultaneous substitutions contain a list of bindings that are applied simultaneously
when they reach a bound variable, for example ``[x=%a, y=x].(x y) ==> (%a x)``.

Recursive substitutions operate like simultaneous substitutions, except that the
whole substitution is re-applied to and substitution result, for example
``[[x=x x]].x ==> [[x=x x]].(x x) ==> ([[x=x x]].x x) ([[x= x x]].x x)``

Lifting specifiers are used to increase the bump counter on variables, for example
``{x^2:4}.(x x^2) ==> (x x^6)``.

The bindings in simultaneous and recursive substitutions can also substitute for
nominal variables, for example ``[x=%a, ?0=%b].(x ?0 x) ==> %a %b %a``.



