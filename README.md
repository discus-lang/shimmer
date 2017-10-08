
# Shimmer

The Reflective Lambda Machine

Shimmer is a minimal Scheme-like language intended as an intermediate representation and compiler target for higher level languages. You can also write programs in it directly, but only if you like parenthesis.

The Shimmer base language is untyped. In typical use the type annotations for object languages are encoded using meta-level constructs and higher order abstract syntax (HOAS).

Shimmer provides support for nominal reasoning, so the lambda abstractions used for HOAS representations can be opened and closed directly. Shimmer also provides explicit substitutions and bumped variables, which means that one can write transformations over object-level abstract syntax with binding without needing to manually handle name capture.


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

Explicit substitutions are used when writing program transformations to avoid the runtime overhead of naive substitution. Bumped variables are used to avoid name capture, for example in the expression ``\x. \x. \x. x x^2`` the variable occurrence ``x`` refers to the inner most parameter, while ``x^2`` refers to  the outermost one.

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
