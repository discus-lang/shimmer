
## Primitives

### Unit

```
  name          params    description
  ----          ------    -----------
  #unit                   unit value
```

``#unit`` is the inert unit value. It is used as a default argument and return value for some of the other primitives.


### Boolean

```
  name          params    description
  ----          ------    -----------
  #true                   boolean true
  #false                  boolean false

  #not          !         boolean negation
  #and          !!        boolean and
  #or           !!        boolean or
  #if           !~~       boolean if-then-else operator
```

``#true`` and ``#false`` are the boolean literals.

``#not``, ``#and`` and ``#or`` are the usual boolean operators.

``#if`` is the usual control structure, with the scrutinee evaluated call-by-value and the 'then' and 'else' branches call-by-name.


### Natural Numbers

```
  name          params    description
  ----          ------    -----------
  #nat'NAT                natural number

  #nat-add      !!        natural addition
  #nat-sub      !!        natural subtration
  #nat-mul      !!        natural multiplication
  #nat-div      !!        natural division
  #nat-rem      !!        natural remainder

  #nat-eq       !!        natural equality
  #nat-neq      !!        natural negated equality
  #nat-lt       !!        natural less than
  #nat-le       !!        natural less than equal
  #nat-gt       !!        natural greater than
  #nat-ge       !!        natural greather than equal
```

Natural number literals are writen like ``#nat'0`` and ``#nat'1234``.

The natural number operators are standard. The ``#nat-add`` through ``#nat-rem`` operators produce natural number results, while the ``#nat-eq`` through ``#nat-ge`` comparison operators produce booleans.


### List

```
  name          params    description
  ----          ------    -----------
  #list                   list constructor

  #list-cons    ~!        add an element to the front of a list
  #list-snoc    !~        add an element to the end of a list
  #list-append  !!        append two lists

  #list-uncons  !~~       split an element from the front of a list
  #list-unsnoc  !~~       split an element from the end of a list
```

List literals are written like ``#list %a %b``. Note that the ``#list`` constructor itself does not evaluate its arguments, so ``#list (#nat-add #nat'2 #nat'4)`` is normal.

``#list-cons %a (#list %b %c) ==> #list %a %b %c``.

Appends an element to the front of a list. If the second argument is not a list then stuck.


``#list-snoc (#list %a %b) %c ==> #list %a %b %c``.

Appends an element to the end of a list. If the first argument is not a list then stuck.


``#list-append (#list %a %b) (#list %c %d) ==> #list %a %b %c %d``.

Appends two lists. If both arguments are not lists then stuck.


``#list-uncons (#list %a %b %c) (\x xs. %split x xs) ==> %split %a (#list %b %c)``.

Splits a list into its head and tail elements and passes both components to a functional argument. If the first argument is not a non-empty list then stuck.

``#list-unsnoc (#list %a %b %c) (\xs x. %split xs x) ==> %split (#list %a %b) %c``.

Splits a list into its last and initial elements and passes both components to a functional argument. If the first argument is not a non-empty list then stuck.


### Nominal

```
  name          params    description
  ----          ------    -----------
  #nom-fresh    !         allocate a fresh nominal variable
  #nom-eq       !!        check equality of two nominal variables
  #nom-close    !~~       creating a closing substitution for a nominal variable
```

``#nom-fresh #unit ==> ?5``

Generate a fresh nominal variable. If the argument is not ``#unit`` then stuck.


``#nom-eq ?0 ?0 ==> #true``

``#nom-eq ?0 ?1 ==> #false``

Tests if two nominal variables are identical. If the arguments are not nominal variables then stuck.


``#nom-close ?0 %a (\x. x ?0 %b) ==> \x. x %a %b``

Close a nominal variable, substituting the second argument for all occurrences of the variable in the third argument. If the first argument is not a nominal variable then stuck.


### Matching

```
  name          params    description
  ----          ------    -----------
  #match-sym    !~~       match a symbol
  #match-app    !~~       match an application
  #match-abs    !~~       match all parameters of an abstraction
  #match-abs1   !~~       match the first parameter of an abstraction
```

``#match-sym %a %no %yes ==> %yes %a``

``#match-sym #nat'5 %no %yes ==> %no``

Test if the first argument is a symbol. If so, apply it to the second argument, otherwise return the third argument.


``#match-app (%a %b %c) %no %yes ==> %yes %a %b %c``

``#match-app %a %no %yes ==> %no``

Test if the first argument is an application. If so, apply the functional expression and the arguments separately to the second argument, otherwise return the third argument.


``#match-abs (\x ~y. %body x y) %no %yes ==> %yes (#list (#list ?0 #true) (#list ?1 #false)) (%body ?0 ?1)``

``#match-abs %a %no %yes ==> %no``

Test if the first argument is an abstraction. If so allocate fresh nominal variables for each parameter, and apply the second argument to two new arguments describing the abstraction. The first new argument is list of pairs consisting of the fresh nominal variables and a flag indicating whether the parameter was call-by-value (``#true``)  or call-by-name (``#false``). The second new argument is the body of the abstraction, with the nominal variables substituted for the parameter variables. If the first argument is not an abstraction then return the third argument.


``#match-abs1 (\x ~y. %body x y) %no %yes ==> %yes (#list ?0 #true) (\~y.%body ?0 y)``

``#match-abs1 %a %no %yes ==> %no``

Like ``#match-abs`` above, but only split the first parameter from the abstraction.

