# TODO: use parametric types with the type parameters corresponding
# to the lexer and parser

abstract type WithLexer end

init_traditional_lexer(p::WithLexer,lexer_conf) = begin
    p.lexer_conf = lexer_conf
    p.lexer = TraditionalLexer(lexer_conf.tokens,ignore=lexer_conf.ignore,user_callbacks=lexer_conf.callbacks)
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

lex(p::WithLexer,text) = begin
    stream = lex(p.lexer,text)
    if p.lexer_conf.postlex != nothing
        return process(p.lexer_conf.postlex,stream)
    end
    return stream
end

parse(p::WithLexer,text) = begin
    token_stream = lex(p,text)
    set_state = partial(set_lexer_state,p.lexer)
    return parse(p.parser,token_stream,set_state=set_state)
end

mutable struct LALR_TraditionalLexer <: WithLexer
    lexer_conf
    lexer
    parser
    LALR_TraditionalLexer(lexer_conf,parser_conf;options=nothing) = begin
        if !isnothing(options) debug = options["debug"] else debug = false end
        x = new()
        x.parser = LALRParser(parser_conf,debug=debug)
        init_traditional_lexer(x,lexer_conf)
        return x
        end
end

mutable struct LALR_ContextualLexer <: WithLexer
    lexer_conf
    lexer
    parser
    LALR_ContextualLexer(lexer_conf,parser_conf;options=nothing) = begin
        if !isnothing(options) debug = options["debug"] else debug = false end
        x = new()
        x.parser = LALRParser(parser_conf,debug=debug)
        init_contextual_lexer(x,lexer_conf)
        return x
        end
end

mutable struct LALR_CustomLexer <: WithLexer
    lexer_conf
    lexer
    parser
    LALR_CustomLexer(lexer_cls,lexer_conf,parser_conf) = begin
        x = new()
        x.parser = LALRParser(parser_conf)
        x.lexer_conf = lexer_conf
        x.lexer = lexer_cls(lexer_conf)
        return x
        end
end

get_ambiguity_options(options) = begin
    if isnothing(options) || options.ambiguity == "resolve"
        return Dict()
    elseif options.ambiguity == "resolve__antiscore_sum"
        return Dict("forest_sum_visitor"=>ForestAntiscoreSumVisitor)
    elseif options.ambiguity == "explicit"
        return Dict("resolve_ambiguity":false)
    end
    error("Bad value: $options")
end

# Enumerate here does not enumerate characters; FIXME

tokenize_text(text) = Channel() do token_chan
    line = 1
    col_start_pos = 1
    for (i, ch) in enumerate(text)
        if '\n' == ch
            line += count(ch,'\n')
            col_start_pos = i + rindex(ch,'\n')
        end
        put!(token_chan, Token("CHAR", ch, line=line, column=i - col_start_pos))
    end
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

# Other parsers left out for now

get_frontend(parser,lexer) = begin
        if parser=="lalr"
            if lexer == nothing
                error("The LALR parser requires use of a lexer")
            elseif lexer == "standard"
                return LALR_TraditionalLexer
            elseif lexer == "contextual"
                return LALR_ContextualLexer
            elseif lexer <: Lexer
                return LALR_CustomLexer(lexer)  #partial application
            else
                error("Unknown lexer: $lexer")
            end
            
        elseif parser=="earley"
            if lexer=="standard"
                return Earley
#==         elseif lexer=="dynamic"
                return XEarley
            elseif lexer=="dynamic_complete"
                return XEarley_CompleteLex   ==#
            elseif lexer=="contextual"
                error("The Earley parser does not support the contextual parser")
            else
                error("Unknown lexer: $lexer")
            end
            
#==        elseif parser == "cyk"
            if lexer == "standard"
                return CYK
            else
                error("CYK parser requires using standard parser.")
            end
==#            
        else
            error("Unknown parser: $parser")
        end
end


    



