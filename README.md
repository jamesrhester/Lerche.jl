![Testing](https://github.com/jamesrhester/Lerche.jl/workflows/Tests/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/jamesrhester/Lerche.jl/badge.svg?branch=master)](https://coveralls.io/github/jamesrhester/Lerche.jl?branch=master)
# Introduction

Lerche (German for Lark) is a partial port of the Lark grammar processor from
Python to Julia.  Lark grammars should work unchanged in Lerche.

# Quick start

See also 'Notes for Lark users' below.

Lerche reads Lark EBNF grammars to produce a parser. This parser, when
provided with text conforming to the grammar, produces a parse
tree. This tree can be visited and transformed using "rules", which
are just Julia methods. The first argument of a rule is an object
which is a subtype of ``Visitor`` or ``Transformer``. The name of a
rule is the production that it should be called on.

1. Define one or more subtypes of ``Transformer`` or ``Visitor`` to be
used to dispatch the appropriate rule. The subtype can also be used to
hold information if you wish, and must be a concrete type at present.
1. Prefix the subtype definition with ``@contains_rules``
1. For every production in your grammar that you wish to transform,
write a rule with identical name to the production
1. The rule should be prefixed with ``@rule`` if the second argument
is an array containing all of the arguments to the grammar production
1. The rule should be prefixed with ``@inline_rule`` if the second
and following arguments refer to each argument in the grammar production
1. Near the top of the module containing your subtypes and rules, put
``@rule_holder`` on a line by itself (may be unnecessary in the future).

For a real-world example of usage, see [this file](https://github.com/jamesrhester/CIF_dREL.jl/blob/master/src/jl_transformer.jl).

# Notes for Lark users

Please read the Lark documentation.  When converting from Lark
programs written in Python to Lerche programs written in Julia, the
changes outlined below are necessary. Note that before version 1.0 use
of the ``@contains_rules`` and ``@rule_holder`` macros is likely to
be simplified.

1. All Transformer and Visitor classes become types
1. All class method calls become Julia method calls with the type as the first argument
(i.e. replacing ``self``)
1. Transformers and visitors should be declared in a single module with the
macro ``@rule_holder`` on a line by itself before any type definitions
1. Transformers and visitors should be declared as subtypes of the appropriate
visitor/transformer type, preceded by the macro ``@contains_rules``
1. Transformation or visitor rules should be preceded by the ``@rule`` macro. Inline
rules use the ``@inline_rule`` macro. 
1. The first argument of transformation and visitor rules is a variable of the
desired transformation/visitor type.
1. Any grammars containing backslash-double quote sequences need to be fixed (see below).
1. Any grammars containing backslash-x to denote a byte value need to be fixed (see below).

## Grammars

Lark grammars should work unchanged in Lerche, with the caveats
below.  Note that this guarantee applies only to the sequence of
characters after interpretation by the Julia/Python language parser.
In particular note the following differences:

1. Raw strings in Julia are written ``raw"<string contents>"`` instead of 
Python's ``r"<string contents>"``

2. The sequence ``\"`` inside a Python raw, quote-delimited string
encodes a two-character sequence.  However, it corresponds to a single
quote in the analogous Julia case. To obtain the two-character
sequence in Julia, input ``\\"``. A backslash-quote sequence is
required in Lark grammars when representing a double quote as the Lark
grammar defines the sequence backslash-quote to represent a quote; so
these two characters must remain in the string after Julia has 
pre-processed it.

3. While unicode escapes are recognised (``\uxxxx``), the Python
``\x`` combination to insert a particular byte value in the
string is not.

4. Avoid using Julia keywords (such as ``true`` or ``false``) as the
names of rules or aliases.  If your Lark grammar does this, you will
need to change it.

# Example

The following example shows how a simple JSON parser is implemented.

First the grammar is written:
```
json_grammar = raw"""
    ?start: value

    ?value: object
          | array
          | string
          | SIGNED_NUMBER      -> number
          | "true"             -> t
          | "false"            -> f
          | "null"             -> null

    array  : "[" [value ("," value)*] "]"
    object : "{" [pair ("," pair)*] "}"
    pair   : string ":" value

    string : ESCAPED_STRING

    %import common.ESCAPED_STRING
    %import common.SIGNED_NUMBER
    %import common.WS

    %ignore WS
"""
```

For details on the grammar syntax refer to the [Lark documentation](https://github.com/lark-parser/lark/blob/master/docs/grammar.md).

Items can be transformed as they are parsed, for example, in order to
immediately turn strings into numbers.  A subtype of ``Transformer``
can be passed as an additional keyword argument when creating the
parser in order to do this.  A method whose name matches the rule
name (or alias) and whose first argument is our subtype of
``Transformer`` will be called whenever that rule is matched.

These methods are prefixed by the ``@rule`` macro (if all of the
parse tree children are collected into a single array argument) or
``@inline_rule`` macro (if the parse tree children have a single
argument each).

```julia
@rule_holder #needed once in the module

@contains_rules struct TreeToJson <: Transformer end

@inline_rule string(t::TreeToJson, s) = replace(s[2:end-1],"\\\""=>"\"")

@rule  array(t::TreeToJson,a) = Array(a)
@rule  pair(t::TreeToJson,p) = Tuple(p)
@rule  object(t::TreeToJson,o) = Dict(o)
@inline_rule number(t::TreeToJson,n) = Base.parse(Float64,n)

@rule  null(t::TreeToJson,_) = nothing
@rule  t(t::TreeToJson,_) = true
@rule  f(t::TreeToJson,_) = false
```

The above rules define a ``TreeToJson`` subtype, and rules whose
names match the rule or alias names in the grammar. For example,
whenever the string rule is matched, the enclosing double quotes
are dropped and any ``\"`` sequences replaced by a double quote.

Finally, we create our parser by calling the ``Lark`` constructor:

```julia
json_parser = Lark(json_grammar, parser="lalr", lexer="standard", transformer=TreeToJson())
```

Now, we can parse JSON by calling the ``Lerche.parse`` method with
``json_parser`` as the first argument and the text to parse as the
second argument:

```
j = Lerche.parse(json_parser,test_json)
```

The above example is available in the Examples directory for
study.

## Other examples

The `tests` directory contains many more very simple examples
of correctly-constructed grammars.


# Inconsistencies with Lark

1. Earley and CYK grammars are not implemented. 
2. Dynamic lexer is not implemented. 
3. All errors with messages attached must be at the bottom of the
exception type hierarchy, as these are the only types that can have
contents. Thus an "UnexpectedInput" exception must become e.g 
an UnexpectedCharacter if a message is included.

# Implementation notes

Lerche is currently noticeably slower than Lark, despite the
advantages of Julia's compilation. There is still plenty of room for
improvement as no effort has been made to use Julia efficiency tricks.
The priority has been on maintaining fidelity with Lark.

Python "yield" has been implemented using Julia Channels.

The ``@rule_holder`` and ``@contains_rules`` macros have been implemented in
order to store rule information in a dictionary local to the calling module. 
So far it has not been possible to store information within Lerche data structures from outside the
``Lerche`` module, and perhaps this is a good thing. Ideally Julia's multiple
dispatch mechanism would be used to select the appropriate rule, but no way
to reliably go from rule name as a string to dispatch on method name has been found.
