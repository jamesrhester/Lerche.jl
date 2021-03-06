module Lerche

using DataStructures  #For Stack when parsing

export Tree, Token, Interpreter,Transformer, visit_children, visit,transform
export GrammarError, ParseError, UnexpectedToken, UnexpectedCharacters
export UnexpectedInput
export Lark
export @inline_rule, @rule
#==

Lerche ("Lark" in German) is a port of the Python Lark package to
native Julia. No I don't speak German.

A number of simplifications have been made:
(1) There is no choice of Tree class. Lark has SlottedTree and Tree,
with the former preferred for speed. This is not a consideration for
Julia.
(2) The individual parsers in the parser modules are given types instead
of using the separate module approach of Python.
  
==#

include("utils.jl")
include("tree.jl")
include("visitors.jl")
include("exceptions.jl")
include("grammar.jl")
include("lexer.jl")
include("common.jl")
include("parse_tree_builder.jl")
include("parsers/grammar_analysis.jl")
include("parsers/lalr_analysis.jl")
include("parsers/lalr_parser.jl")
include("parser_frontends.jl")
include("load_grammar.jl")
include("lark.jl")

# Compatibility for Julia < 1.4

try
    TaskFailedException
catch ex
    struct TaskFailedException <: Exception
    end
end     

# Prepare the Lark EBNF parser

const _lark_grammar = GrammarLoader()

load_grammar(text::String;options...) = load_grammar(_lark_grammar,text;options...)

end # module
