"""This module implements a LALR(1) Parser
"""
# Author: Erez Shinan (2017)
# Email : erezshin@gmail.com

# Adapted to Julia by James Hester (2019)
# Email : james.r.hester@gmail.com

export feed_token!

struct ParseConf
    parse_table::ParseTable
    start_state::Int64
    end_state::Int64
    states::Array{Dict{String,Tuple{Symbol,Any}},1}
    callbacks::Dict{Rule,Function}
    start::String
end

struct _Parser
    parse_table::ParseTable
    callbacks::Dict{Rule,Function}
    debug::Bool
end

_Parser(parse_table,callbacks;debug=false) = begin
    _Parser(parse_table,callbacks,debug)
end

ParseConf(parse_table,callbacks,start) = begin
    ParseConf(parse_table,parse_table.start_states[start],
              parse_table.end_states[start],
              parse_table.states,
              callbacks,
              start)
end

struct LALRParser
    _parse_table::ParseTable
    parser_conf::ParserConf
    parser::_Parser
    debug::Bool
end

LALRParser(parser_conf;debug=false) = begin
    analysis = LALR_Analyzer(parser_conf,debug=debug)
    compute_lalr!(analysis)
    callbacks = parser_conf.callbacks
    parser = _Parser(analysis.parse_table,callbacks)
    LALRParser(analysis.parse_table,parser_conf,parser,debug)
end

parse(l::LALRParser,args...;kwargs...) = begin
    #println("Parsing with $args, $kwargs")
    parse(l.parser,args...,kwargs...)
end

struct ParserState
    parse_conf::ParseConf
    state_stack::Stack{Int64}
    value_stack::Stack{Any}
end

ParserState(parse_conf;state_stack=nothing,value_stack=nothing) = begin
    if state_stack === nothing
        state_stack = Stack{Int64}()
        push!(state_stack,parse_conf.start_state)
    end
              
    ParserState(parse_conf, state_stack,
                value_stack === nothing ? Stack{Any}() : value_stack
                )
end

position(ps::ParserState) = first(ps.state_stack)

feed_token!(ps::ParserState,token;is_end=false) = begin
    state_stack = ps.state_stack
    value_stack = ps.value_stack
    states = ps.parse_conf.states
    end_state = ps.parse_conf.end_state
    callbacks = ps.parse_conf.callbacks
    while true
        arg = nothing  #create outside scope of try
        action = nothing # ditto
        state = first(state_stack)
        @debug "State $state, current token $(token.type_) ($token)"
        try
            @debug "Possibles: $(keys(states[state]))\n"
            action,arg = states[state][token.type_]
        catch e
            if e isa KeyError
                expected = [s for s in keys(states[state]) if all(x->isuppercase(x),s)]
                throw(UnexpectedToken(token, expected, state=state, puppet=nothing))
            else
                rethrow(e)
            end
        end
        @assert arg != end_state
        if action == :shift
            @assert !is_end
            @debug "Shift!"
            push!(state_stack,arg)
            push!(value_stack,token)
            return
        end
        size = length(arg.expansion)::Int
        @debug "Preparing to pop $size values"
        if size > 0
            s = grab!(value_stack,size)
            for i = 1:size
                pop!(state_stack)
            end
        else
            s = []
        end
        value = callbacks[arg](s)

        @debug "Reduced, value now $value"
        _action, new_state = states[first(state_stack)][arg.origin.name]::Tuple{Symbol,Int64}
        @assert _action == :shift
        push!(state_stack,new_state)
        push!(value_stack,value)
        if is_end && first(state_stack) == end_state
            return first(value_stack)
        end
    end
end

parse(p::_Parser,lexer::LexerThread,start::String; value_stack = nothing, state_stack=nothing) = begin
    parse_conf = ParseConf(p.parse_table,p.callbacks,start)
    parser_state = ParserState(parse_conf,state_stack=state_stack,value_stack=value_stack)
    return parse_from_state(p,lexer,parser_state)
end

parse_from_state(p::_Parser,lexer,state::ParserState) = begin
    # Main LALR-parser loop
    lp = LexerParser(lexer,state)
    try
        token = nothing
        for token in lp
            @debug println("Seen $(token.type_): $token")
            feed_token!(state,token)
        end
        token = !(token===nothing) ? new_borrow_pos("\$END","",token) : Token("\$END","",
                                                                              pos_in_stream=0,
                                                                              line=1,
                                                                              column=1)
        return feed_token!(state,token,is_end=true)
    catch e
        if e isa UnexpectedInput
            @debug "Unexpected input: $e"
            try
                e.puppet = ParserPuppet(p,state,lexer)
            catch f
                if !(f isa UndefVarError) #parser puppet might not be defined
                    throw(f)
                end
            end
            rethrow(e)
        else
            @debug begin
                println("")
                println("STATE STACK DUMP")
                println("----------------")
                for (i,s) in enum(state.state_stack)
                    println("$i)$s")
                end
                println("")
            end
            rethrow(e)
        end
    end
end

