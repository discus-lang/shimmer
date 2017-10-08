
## Grammar

### Declarations

```
Decl  ::=  '@' Name Param* '=' Exp ';'    (Macro declaration)
```

A declaration consists of a macro name prefixed by '@', the macro parameters and
the bound expression. All declarations end with a ';'.


### Expressions

```
Exp   ::=  Ref                            (External reference)
       |   Key Exp                        (Keyword  application)
       |   Exp Exp+                       (Function application)
       |   Name ('^' Nat)?                (Variable with lifting specifier)
       |   '\' Param+ '.' Exp             (Function abstraction)
       |   Train      '.' Exp             (Substitution train)
``

Expressions consist of references, keyword applications, function applications,
variables, abstractions and substitution trains.

References to top-level external things are have a different form to variables as
there is no possiblity of name capture during substitution.

Keyword applications are used when applying keywords as they only take one argument
expression at a tome.

Function applications consists of a functional expression applied to a list of arguments.

Variables consists of a name and a bump count. If the bump count is zero then it is
elided. The bump count is used to manage name clashes during substitution, for example
in the expression ``(\x. \x. x^1)` the variable reference ``x^1`` referes to the outer
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


## Keywords

```
Key   ::= '##tag'                         (Tag an expression)
       |  '##seq'                         (Sequence evaluation)
       |  '##box'                         (Box an expression, delaying evaluation)
       |  '##run'                         (Run an expression, forcing  evaluation)
```


## Parameters

```
Param ::= Name                            (Call-by-value parameter)
       |  '!' Name                        (Explicitly call-by-value parameter)
       |  '~' Name                        (Explicitly call-by-name  parameter)
```




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


