# Shimmer
The Lambda Machine


Declarations
~~~~~~~~~~~~

```
Decl ::=  '@' Name Param* '=' Exp ';'    (Macro declaration)
      |   '+' Name        '=' Exp ';'    (Set binding declaration)
```


Expressions
~~~~~~~~~~~

```
Exp  ::=  Ref                     (External reference)
      |   Exp Exp                 (Function application)
      |   Key Exp                 (Keyword  application)
      |   Name ('^' Nat)?         (Variable with lifting specifier)
      |   '\' Param+ '.' Exp      (Function abstraction)
      |   Train      '.' Exp      (Substitution train)
      |   '<' Exp,+ '>'           (Return multiple expressions)

Ref  ::= '@' Name                 (Macro reference)
      |  '+' Name                 (Set reference)
      |  '%' Name                 (Symbol reference)
      |  '#' Name                 (Primitive reference)
      
Key  ::= 'tag'                    (Tag an expression)
      |  'seq'                    (Sequence evaluation)
      |  'box'                    (Box an expression, delaying evaluation)
      |  'run'                    (Run an expression, forcing  evaluation)

Param ::= Name                    (Call-by-value parameter)
       |  '!' Name                (Explicitly call-by-value parameter)
       |  '~' Name                (Explicitly call-by-name  parameter)
```

Substitutions
~~~~~~~~~~~~~

```
Train ::= Car+                    (Substitution train)

Car   ::= '['  Bind,* ']'         (Simultaneous substitution)
       |  '[[' Bind,* ']]'        (Recursive substitution)
       |  '{'  Bump,* '}'         (Lifting specifier)
       
Bind  ::= Name ('^' Nat)? '=' Exp
Bump  ::= Name ('^' Nat)? ':' Nat
```


Syntactic Sugar
~~~~~~~~~~~~~~~

```
 Exp '$' Exp Exp  =  Exp (Exp Exp)
```

