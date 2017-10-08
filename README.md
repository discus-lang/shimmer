
# Shimmer

The Lambda Machine

Shimmer is a minimal Scheme-like language intended to be used as an intermediate representation and compiler target for other languages. You can also write programs in it directly, but only if you like parenthesis.


## Grammar

The basic expression grammar is as follows:

```
Decl  ::=  '@' Name Param* '=' Exp ';'    (Macro declaration)

Exp   ::=  Ref                            (External reference)
       |   Key Exp                        (Keyword  application)
       |   Exp Exp+                       (Function application)
       |   Name ('^' Nat)?                (Variable with lifting specifier)
       |   '\' Param+ '.' Exp             (Function abstraction)
       |   Train      '.' Exp             (Substitution train)
```

See the ``Grammar.md`` file under ``./docs`` for more information.

Shimmer is particuarly well suited as the intermediate representation for other functional languages due to the minimal expression grammar, as well as inclusion of explicit substitutions with bumped variables, as well as nominal reasoning.

Explicit substitutions are used when writing program transformations to avoid the runtime overhead of naive substitution. Bumped variables are used to avoid name capture, for example in the expression ``\x. \x. \x. x x^2`` the variable occurrence ``x`` refers to the inner most parameter, while ``x^2`` refers to  the outermost one.

Nominal reasoning allows lambda abstractions to be safely opened and closed when writing program transformations over higher order abstract syntax. For example, we can write a Shimmer program that takes an arbitrary Shimmer function of two parameters and swaps the order of the parameters, modifying the abstract syntax tree itself.

Shimmer also contains a variety of well defined primitive operations such as for booleans, natural numbers, and several matching forms. All numeric operators have explicit prefixes giving the type they operate at, which makes the syntax unambiguous and easy to parse by third party tools.


## Getting Started

Here is a simple program, to compute the factorial of a number using an accumulator:

```
@fac-acc x
 = @fac-acc-loop #nat'1 x;

@fac-acc-loop acc x
 = #if  (#nat-eq x #nat'0)
        acc
        (@fac-acc-loop (#nat-mul x acc) (#nat-sub x #nat'1));
```

Names starting with ``@`` refer to top-level macros that are expanded during evaluation as needed. Names starting with ``#`` refer to primitive values and operators. Once we add the above declarations to a file ``Factoral.smr`` we can used the Shimmer REPL to evaluate them, including generating a trace of single step evaluation.

```
$ shimmer Factorial.smr

Shimmer, version 0.1. The Lambda Machine.
Type :help for help.

> @fac-acc #nat'5
#nat'120

> :trace @fac-acc #nat'5
(\x.@fac-acc-loop #nat'1 x) #nat'5
[x=#nat'5].@fac-acc-loop #nat'1 x
@fac-acc-loop #nat'1 #nat'5
...
#nat'120

```
