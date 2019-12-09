struct LexerConf
    tokens::Array{TerminalDef}
    ignore::Array{String}
    postlex  #Call the "process" method on this with the stream after lexing
    callbacks::Dict{String,Function}
end

LexerConf(tokens;ignore=(),postlex=nothing,callbacks=Dict()) = LexerConf(tokens,ignore,postlex,callbacks)

struct ParserConf
    rules::Array{Rule}
    callback::Dict{String,Function}
    start::String #The name of the starting rule
end

