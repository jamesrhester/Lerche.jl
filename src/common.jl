struct LexerConf
    tokens::Array{TerminalDef}
    ignore::Array{String}
    postlex  #Call the "process" method on this with the stream after lexing
    callbacks::Dict{String,Function}
    g_regex_flags::String
    skip_validation::Bool
end

LexerConf(tokens;ignore=(),postlex=nothing,callbacks=Dict(),g_regex_flags=0,skip_validation=false) = begin
    # convert Lark/Python integer flags back to characters
    new_flags = ""
    if (g_regex_flags & 256) > 0
        new_flags *='a'  #ascii
    end
    if (g_regex_flags & 2 ) > 0
        new_flags *= 'i'  #ignore case
    end
    if (g_regex_flags & 8 ) > 0
        new_flags *= 'm'   #multiline
    end
    if (g_regex_flags & 16) > 0
        new_flags *= 's'   #dotall
    end
    if (g_regex_flags & 64) > 0
        new_flags *= 'x'   #verbose
    end
    LexerConf(tokens,ignore,postlex,callbacks,new_flags,skip_validation)
end

reconfigure(l::LexerConf,tokens) = begin
    LexerConf(tokens,l.ignore,l.postlex,l.callbacks,l.g_regex_flags,l.skip_validation)
end

struct ParserConf
    rules::Vector{Rule}
    callbacks::Dict{Rule,Function}
    start::Vector{String} #The names of the starting rule
end

