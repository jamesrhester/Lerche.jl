![Testing](https://github.com/jamesrhester/Lerche.jl/workflows/CI/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/jamesrhester/Lerche.jl/badge.svg?branch=master)](https://coveralls.io/github/jamesrhester/Lerche.jl?branch=master)
# Introduction

Lerche (German for Lark) is a partial port of the Lark grammar processor from
Python to Julia.  Lark grammars should work unchanged in Lerche.

**Installation**: at the Julia REPL, `using Pkg; Pkg.add("Lerche")`

**Documentation**: [![][docs-stable-img]][docs-stable-url] [![][docs-latest-img]][docs-latest-url]

# Quick start

See also 'Notes for Lark users' below.

Lerche reads Lark EBNF grammars to produce a parser. This parser, when
provided with text conforming to the grammar, produces a parse
tree. This tree can be visited and transformed using "rules". A rule is
a function named after the production whose arguments it should be called on, and
the first argument of a rule is an object which is a subtype of
``Visitor`` or ``Transformer``.

Given an EBNF grammar, it can be used to parse text into your data
structure as follows:
1. Define one or more subtypes of ``Transformer`` or ``Visitor`` instances of which will be
passed as the first argument to the appropriate rule. The instance can also be used to
hold information during transformation if you wish, in which case it must have a concrete type.
1. Define `visit_tokens(t::MyNewType) = false`. This is currently an order of magnitude faster
than leaving the default `true`.
1. For every production in your grammar that you wish to process,
write a rule with identical name to the production
1. The rule should be prefixed with macro ``@rule`` if the second argument
is an array containing all of the arguments to the grammar production
1. The rule should be prefixed with macro ``@inline_rule`` if the second
and following arguments refer to each argument in the grammar production

If your grammar is in ``String`` variable ``mygrammar``, your text to be parsed and transformed
is in ``String`` variable ``mytext``, and your ``Transformer`` subtype is ``MyTransformer``, the
following commands will produce a data structure from the text:

```julia
using Lerche
p = Lark(mygrammar,parser="lalr",lexer="contextual") #create parser
t = Lerche.parse(p,mytext)     #Create parse tree
x = Lerche.transform(MyTransformer(),t)  #transform parse tree
```

For a real-world example of usage, see [this file](https://github.com/jamesrhester/DrelTools.jl/blob/master/src/jl_transformer.jl).

# Notes for Lark users

Please read the Lark documentation.  When converting from Lark
programs written in Python to Lerche programs written in Julia, the
changes outlined below are necessary.

1. All Transformer and Visitor classes become types
1. All class method calls become Julia method calls with an instance of the type as the first argument
(i.e. replacing ``self``)
1. Transformation or visitor rules should be preceded by the ``@rule`` macro. Inline
rules use the ``@inline_rule`` macro. 
1. The first argument of transformation and visitor rules is a variable of the
desired transformation/visitor type.
1. Any grammars containing backslash-double quote sequences need to be fixed (see below).
1. Any grammars containing backslash-x to denote a byte value need to be fixed (see below).

## Inconsistencies with Lark

1. Earley and CYK grammars are not implemented. 
2. Dynamic lexer is not implemented. 
3. All errors with messages attached must be at the bottom of the
exception type hierarchy, as these are the only types that can have
contents. Thus an ``UnexpectedInput`` exception must become e.g 
an ``UnexpectedCharacter`` exception if a message is included.
4. The `PuppetParser` invoked when there is a parse error is not yet
functional
5. There may be issues with correctly interpreting import paths
to find imported grammars: please raise an issue if this happens.
6. No choice of ``regex`` engine, ``Tree`` structure or byte/string
choices are available as they make no sense for Julia.

# Implementation notes and hints

Lerche is currently based off Lark 0.11.1. The priority has been on
maintaining fidelity with Lark. For example, global `regex` flags
which are integers in Lark are still integers in Lerche, which means
you will need to look their values up. This may be changed to a more
Julian approach in future.

The ``@rule`` and ``@inline_rule`` macros define methods of Lerche function
`transformer_func`. Julia multiple dispatch is used to select the
appropriate method at runtime.

Parsing a large (500K) file suggest Lerche is about 3 times faster
than Lark for parsing. Parser generation is much slower as no
optimisation techniques have been applied (yet). Calculating and
storing your grammar in a Julia `const` variable at the top level 
of your package will allow it to be precompiled and thus avoid
grammar re-analysis each time your package is loaded.

[docs-latest-img]: https://img.shields.io/badge/docs-latest-blue.svg
[docs-latest-url]: http://jamesrhester.github.io/Lerche.jl/dev/

[docs-stable-img]: https://img.shields.io/badge/docs-stable-blue.svg
[docs-stable-url]: http://jamesrhester.github.io/Lerche.jl/stable/
