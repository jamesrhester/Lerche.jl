# TODO: use parametric types with the type parameters corresponding
# to the lexer and parser

get_frontend(parser,lexer) = begin
    if parser=="lalr"
        if lexer === nothing
            throw(error("The LALR parser requires use of a lexer"))
        elseif lexer == "standard"
            return LALR_TraditionalLexer
        elseif lexer == "contextual"
            return LALR_ContextualLexer
        else
            throw(error("Unknown lexer: $lexer"))
        end
    else
        throw(error("Unknown parser: $parser"))
    end
end

#TODO: our transformers are structured differently
#==
_get_lexer_callbacks(transformer,terminals) = begin
    result = Dict()
    for terminal in terminals
        callback = getproperty(transformer, Symbol(terminal.name), nothing)
        if callback != nothing
            result[terminal.name] = callback
        end
    end
    return result
end
==#

struct PostLexConnector
    lexer
    postlexer
end

make_lexer_state(pc::PostLexConnector,text) = make_lexer_state(pc.lexer,text)

# TODO: use an iteration approach
lex(pc::PostLexConnector,lexer_state,parser_state) = begin
    i = lex(pc.lexer,lexer_state,parser_state)
    return process(pc.postlexer,i)
end

abstract type _ParserFrontend end
abstract type WithLexer <: _ParserFrontend end

_parse(pf::_ParserFrontend,start,input,args...) = begin
    if start === nothing
        start = pf.start
        if length(start) > 1
            throw(error("Lark initialized with more than 1 possible start rule. Must specify which start rule to parse: $start"))
        end
        start = start[]
    end
    parse(pf.parser,input, start,args...)
end

make_lexer(wl::WithLexer,text) = begin
    lexer = wl.lexer
    if wl.postlex != nothing
        lexer = PostLexConnector(wl.lexer,wl.postlex)
    end
    return LexerThread(lexer,text)
end

parse(wl::WithLexer,text;start=nothing) = begin
    _parse(wl,start,make_lexer(wl,text))
end

abstract type LALR_WithLexer <: WithLexer end

struct LALR_TraditionalLexer <: LALR_WithLexer
    lexer
    lexer_conf
    postlex
    parser
    start
end

LALR_TraditionalLexer(lexer_conf,parser_conf;options=nothing) = begin
    debug = options !== nothing ? options.debug : false 
    LALR_TraditionalLexer(TraditionalLexer(lexer_conf), lexer_conf,
                          lexer_conf.postlex,
                          LALRParser(parser_conf,debug=debug),
                          parser_conf.start
                          )
end

struct LALR_ContextualLexer <: LALR_WithLexer
    lexer
    lexer_conf
    postlex
    parser
    start
end

LALR_ContextualLexer(lexer_conf,parser_conf;options=nothing) = begin
    debug = options !== nothing ? options.debug : false 
    parser = LALRParser(parser_conf,debug=debug)
    postlex = lexer_conf.postlex
    states = Dict([idx=>collect(keys(t)) for (idx,t) in parser._parse_table.states])
    always_accept = postlex !== nothing ? postlex.always_accept : ()
    lexer = ContextualLexer(lexer_conf,states,always_accept=always_accept)
    LALR_ContextualLexer(lexer,lexer_conf,postlex,parser,parser_conf.start)
end

init_contextual_lexer(p::WithLexer,lexer_conf) = begin
    p.lexer_conf = lexer_conf
    states = Dict(idx=>collect(keys(t)) for (idx,t) in p.parser._parse_table.states)
    always_accept = if !isnothing(lexer_conf.postlex)
        lexer_conf.postlex.always_accept
    else
        ()
    end
    state_channel = Channel(0)
    p.lexer = ContextualLexer(lexer_conf.tokens,states,state_channel,
                              ignore=lexer_conf.ignore,
                              always_accept=always_accept,
                              user_callbacks=lexer_conf.callbacks)
end

# Note we don't have to pass the match method as multiple despatch
# will pick the right match to use
struct Earley <: WithLexer
    lexer_conf
    lexer
    parser
    Earley(lexer_conf,parser_conf,options) = begin
        x = new()
        init_traditional_lexer(x,lexer_conf)
        x.parser = earley.Parser(parser_conf,match,get_ambiguity_options(options)...)
        return x
    end    
end

match(e::Earley,term,token) = term.name == token.type_




