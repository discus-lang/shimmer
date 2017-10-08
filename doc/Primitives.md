
## Primitives

### Unit

```
  name          params    description
  ----          ------    -----------
  #unit                   unit value
```


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


### List

```
  name          params    description
  ----          ------    -----------
  #list                   list constructor

  #list-cons    ~!        add an element to the front of a list
  #list-uncons  !~~       split an element from the front of a list
  #list-snoc    !~        add an element to the end of a list
  #list-unsnoc  !~~       split an element from the end of a list
  #list-append  !!        append two lists
```


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

