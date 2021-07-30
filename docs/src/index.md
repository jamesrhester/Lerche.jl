# Introduction: Lerche.jl

Lerche.jl creates a parser for a language specified in an EBNF-like
syntax. The resulting parse trees can be transformed using
easy-to-specify methods, where Lerche.jl takes care of the parse tree
traversal. Lerche.jl is a direct translation of the Python-language
Lark parser generator (Lerche is "Lark" in German).

Much of the extensive [Lark
documentation](https://lark-parser.readthedocs.io/) is also relevant.

# Quick start

If you are already familiar with Lark see 
[Notes for Lark users](#Notes-for-Lark-users) below.

Lerche reads EBNF grammars as recognised by Lark
("Lark grammars") to produce a parser. This parser, when provided with
text conforming to the grammar, produces a parse tree. This tree can
be visited and transformed using "rules". A rule is a function named
after the production whose arguments it should be called on, and the
first argument of a rule is an object which is a subtype of
[`Visitor`](@ref) or [`Transformer`](@ref).

Given an EBNF grammar, it can be used to parse text into your data
structure as follows:

  1. Define one or more subtypes of [`Transformer`](@ref) or
     [`Visitor`](@ref), instances of which will be passed as the first
     argument to the appropriate rule.  The instance can also be used
     to hold information during transformation if you wish, in which
     case it must have a concrete type.
  1. Define `visit_tokens(t::MyNewType) = false` if you will not be
     processing token values. This is about 25% faster than leaving the
     default `true`.
  1. For every production in your grammar that you wish to process,
     write a rule with identical name to the production
  1. The rule should be prefixed with macro [`@rule`](@ref) if the second argument
     is an array containing all of the arguments to the grammar production
  1. The rule should be prefixed with macro [`@inline_rule`](@ref) if the second
     and following arguments refer to each argument in the grammar production
  1. For every token which you wish to process, define an identically-named method
     as for rules, but precede it with a [`@terminal`](@ref) macro instead of `@rule`.


If your grammar is in `String` variable `mygrammar`, your text to be parsed and transformed
is in `String` variable `mytext`, and your `Transformer` subtype is `MyTransformer`, the
following commands will produce a data structure from the text:

```julia
p = Lark(mygrammar,parser="lalr",lexer="contextual") #create parser
t = Lerche.parse(p,mytext)     #Create parse tree
x = Lerche.transform(MyTransformer(),t)  #transform parse tree
```

For a real-world example of usage, see [this
file](https://github.com/jamesrhester/DrelTools.jl/blob/master/src/jl_transformer.jl).

!!! tip

    Fully qualify the `parse` call (i.e. write `Lerche.parse`) to avoid ambiguity with `parse` from
    other packages, including `Base.parse`
    
## Error handling

When the supplied text does not match the grammar, `parse` raises exceptions that
are subtypes of `UnexpectedInput`:[`UnexpectedToken`](@ref)
and [`UnexpectedCharacters`](@ref). `Base.show` for these types produces an informative
message regarding the position of the error and expected tokens.

# Defining a Grammar

The full Lark grammar is described [here](grammar.md).

# Example

The following example (condensed from [the JSON example in
Lark](https://lark-parser.readthedocs.io/en/latest/json_tutorial.html) )
how a simple JSON parser is implemented.

```@setup json
using Lerche
```

First, the grammar:
```@repl json
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
""";
```

Note that terminals are always uppercase, and common definitions
can be imported from definitions in the standard library supplied with
Lerche.

A method whose name matches the rule name (or alias) and whose first
argument has our subtype of [`Transformer`](@ref) will be called
whenever that rule is matched.

These methods are prefixed by the [`@rule`](@ref) macro (if all of the
parse tree children are collected into a single array argument) or
[`@inline_rule`](@ref) macro (if each parse tree child is assigned a
separate argument). 

```@example json

struct TreeToJson <: Transformer end

@inline_rule string(t::TreeToJson, s) = replace(s[2:end-1],"\\\""=>"\"")

@rule  array(t::TreeToJson,a) = Array(a)
@rule  pair(t::TreeToJson,p) = Tuple(p)
@rule  object(t::TreeToJson,o) = Dict(o)
@inline_rule number(t::TreeToJson,n) = Base.parse(Float64,n)

@rule  null(t::TreeToJson,_) = nothing
@rule  t(t::TreeToJson,_) = true
@rule  f(t::TreeToJson,_) = false
```

The above rules define a `TreeToJson` subtype of `Transformer`, and rules whose
names match the rule or alias names in the grammar. For example,
whenever the `string` rule is matched, the enclosing double quotes
are dropped and any `\"` sequences replaced by a double quote.

Finally, we create our parser by calling the `Lark` constructor:

```@repl json
json_parser = Lark(json_grammar, parser="lalr", lexer="standard", transformer=TreeToJson());
```

Passing the `transformer` argument at parser construction time avoids
a separate call to the [`transform`](@ref) method after parsing.

Now, we can parse JSON by calling the [`Lerche.parse`](@ref) method with
`json_parser` as the first argument and the text to parse as the
second argument:

```@repl json
text = raw"{\"key\": [\"item0\", \"item1\", 3.14]}"
j = Lerche.parse(json_parser,text)
```

The above example is available in the Examples directory for
study.

## Other examples

The `tests` directory contains many more very simple examples
of correctly-constructed grammars.

# Notes for Lark users

When converting from Lark programs written in Python to Lerche
programs written in Julia, make the following changes:

  1. All Transformer and Visitor classes become types  
  2. All class method calls become Julia method calls with an instance
     of the type as the first argument (i.e. replacing `self`)
  3. Transformation or visitor rules should be preceded by the
     [`@rule`](@ref) macro. Inline rules use the [`@inline_rule`](@ref)
     macro and token processing methods use [`@terminal`](@ref). 
  4. Any grammars containing backslash-double quote sequences need to be edited (see below).
  5. Any grammars containing backslash-x to denote a byte value need to be edited (see below).

## Grammars

Lark grammars work unchanged in Lerche, with the caveats below.  Note
that this guarantee applies to the sequence of characters after
interpretation by the Julia or Python language parser.  In particular
note the following differences:

  1. Raw strings in Julia are written `raw"<string contents>"` instead
     of Python's `r"<string contents>"`
  2. The sequence `\"` inside a Python raw, quote-delimited string
     encodes a two-character sequence.  However, it corresponds to a
     single quote in Julia. To obtain the two-character sequence in
     Julia, write `\\"`. Such a backslash-quote sequence is required
     in Lark grammars to represent a double quote, just like in
     Python; so these two characters must remain in the string after
     Julia has pre-processed it.
  3. While unicode escapes are recognised (`\uxxxx`), the Python `\x`
     combination to insert a particular byte value in the string is not.
     Simply replace with the appropriate Unicode character.

