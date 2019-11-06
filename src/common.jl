struct LexerConf
    tokens
    ignore
    postlex
    callbacks
end

LexerConf(tokens;ignore=(),postlex=nothing,callbacks=Dict()) = LexerConf(tokens,ignore,postlex,callbacks)

struct ParserConf
    rules
    callback
    start
end

