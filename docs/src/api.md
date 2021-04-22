# API Documentation

```@meta
CurrentModule = Lerche
```
## Parsing

A parser is created from a Lark grammar by calling the Lark constructor. Parsing
is initiated by calling `parse`.

```@docs
Lark(grammar::String;options...)
Lark(grammar::IOStream,source;options...)
Lerche.open(grammar_filename;rel_to=nothing,options...)
Lerche.parse(l::Lark,text;start=nothing,on_error=nothing)
```

## Working with the parse tree

`transform` transforms the parse tree according to rules defined by
the user using `@rule` and `@inline_rule` macros. Tokens will also be
processed if `visit_tokens` returns `true` for that transformer type. 
This will significantly slow down parse tree processing.

```@docs
@rule s
@inline_rule s
Transformer
Transformer_InPlace
Transformer_InPlaceRecursive
visit_tokens(t::Transformer)
transform(tr::Transformer,tree)
Visitor
Visitor_Recursive
Interpreter
visit(v::Visitor,tree)
```
