
## Primitives

### Unit

```
  name          params    description
  ----          ------    -----------
  #unit                   unit value
```

``#unit`` is the inert unit value. It is used as a default argument and return value for some of the other primitives.


### Bool

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

``#list-cons`` appends an element to the front of a list.
``#list-cons %a (#list %b %c) ==> #list %a %b %c``.

``#list-snoc`` appends an element to the end of a list.
``#list-snoc (#list %a %b) %c ==> #list %a %b %c``.

``#list-append`` appends two lists.
``#list-append (#list %a %b) (#list %c %d) ==> #list %a %b %c %d``.

``#list-uncons`` splits a list into its head and tail elements and passes both components to a functional argument. If the list is empty then computation is stuck.
``#list-uncons (#list %a %b %c) (\x xs. %split x xs) ==> %split %a (#list %b %c)``.

``#list-uncons`` splits a list into its last and initial elements and passes both components to a functional argument. If the list is empty then computation is stuck.
``#list-unsnoc (#list %a %b %c) (\xs x. %split xs x) ==> %split (#list %a %b) %c``.



### Nominal

```
  name          params    description
  ----          ------    -----------
  #nom-eq       !!        check equality of two nominal variables
  #nom-fresh    !         allocate a fresh nominal variable
  #nom-close    !~~       creating a closing substitution for a nominal variable
```


### Matching

```
  name          params    description
  ----          ------    -----------
  #match-sym    !~~       match a symbol
  #match-app    !~~       match an application
  #match-abs    !~~       match all parameters of an abstraction
  #match-abs1   !~~       match the first parameter of an abstraction
```

