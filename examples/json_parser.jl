#
# This example shows how to write a basic JSON parser
#
# Adapted from the Lark example
#

using Lerchen
using JSON

# Note the change in grammar vs Python: we do not
# use 'true' and 'false' as the transformation
# functions to avoid name collisions.

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


struct TreeToJson <: Transformer end

# Do we need this?
@inline_rule string(t::TreeToJson, s) = replace(s[2:end-1],"\\\""=>"\"")

@rule  array(t::TreeToJson,a) = Array(a)
@rule  pair(t::TreeToJson,p) = Tuple(p)
@rule  object(t::TreeToJson,o) = Dict(o)
@inline_rule number(t::TreeToJson,n) = Base.parse(Float64,n)

@rule  null(t::TreeToJson,_) = nothing
@rule  t(t::TreeToJson,_) = true
@rule  f(t::TreeToJson,_) = false

# json_parser = Lark(json_grammar, parser='earley', lexer='standard')
# def parse(x):
#     return TreeToJson().transform(json_parser.parse(x))

json_parser = Lark(json_grammar, parser="lalr", lexer="standard", transformer=TreeToJson())

test() = begin
    test_json = """
        {
            "empty_object" : {},
            "empty_array"  : [],
            "booleans"     : { "YES" : true, "NO" : false },
            "numbers"      : [ 0, 1, -2, 3.3, 4.4e5, 6.6e-7 ],
            "strings"      : [ "This", [ "And" , "That", "And a \\\"b" ] ],
            "nothing"      : null
        }
    """

    j = Lerchen.parse(json_parser,test_json)
    println(j)
    k = JSON.parse(test_json)
    println(k)
    @assert j == k
end

# Add a reader here if necessary
test()
