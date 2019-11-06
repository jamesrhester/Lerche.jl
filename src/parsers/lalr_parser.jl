"""This module implements a LALR(1) Parser
"""
# Author: Erez Shinan (2017)
# Email : erezshin@gmail.com

# Adapted to Julia by James Hester (2019)
# Email : james.r.hester@gmail.com

struct Parser
    _parse_table
    parser_conf
    parser
    parse
end

Parser(parser_conf;debug=false) = begin
    @assert all([r.options == nothing || r.options.priority == nothing for r in parser_conf.rules])
    analysis = LALR_Analyzer(parser_conf,debug=debug)
    analysis.compute_lookahead()
    callbacks = Dict([("rule", get(parser_conf.callback,
                                   if rule.alias != nothing else rule.origin end,
                                   nothing)
                       ) for rule in parser_conf.rules])
    parser = _Parser(analysis.parse_table,callbacks)
    Parser(analysis.parse_table,parser_conf,parser,parser.parse)
end

struct _Parser
    states
    start_state
    end_state
    callbacks
end

_Parser(parse_table,callbacks) = begin
    _Parser(parse_table.states,parse_table.start_state,parse_table.end_state,callbacks)
end

parse(p::_Parser,seq;set_state = nothing, debug=false) = begin
    i = 0
    token = nothing
#    stream = iter(seq)   ####
    states = p.states

    state_stack = [p.start_state]
    value_stack = []

    if set_state!=nothing set_state(p.start_state) end

    print_state(state_element) = begin
        if hasproperty(state_element,:pretty)
            return state_element.pretty()
        else
            return String(state_element)
        end
    end
    
    get_action(token) = begin
        state = state_stack[end]
        try
            return states[state][token.type_]
        catch e
            if e isa KeyError
                expected = [s for s in keys(states[state]) if isuppercase(s)]
                throw(UnexpectedToken(token, expected, state=state))  #
            else
                rethrow(e)
            end
        end
    end
    
    reduce(rule) = begin
        size = length(rule.expansion)
        if size>0
            s = value_stack[1:end-size]
            state_stack = state_stack[1:end-size]
            value_stack = value_stack[1:end-size]
        else
            s = []
        end
        
        value = p.callbacks[rule](s)

        _action, new_state = states[state_stack[end-1]][rule.origin.name]
        @assert _action == Shift
        push!(state_stack,new_state)
        push!(value_stack,value)
    end

    # Main LALR-parser loop
    for token in seq
        while true
            action, arg = get_action(token)
            @assert arg != p.end_state
            
            if action == Shift
                if debug
                    println("Seen $(token.type),\n shifting to state $arg")
                end
                append!(state_stack,arg)
                append!(value_stack,token)
                if set_state != nothing
                    set_state(arg)
                end
                break # next token
            else
                if debug
                    println("Seen $(token.type),\n reducing ")
                end
                reduce(arg)
                if debug
                    print("Reduced to \n $(state_stack[end])")
                    pretty = [print_state(d) for d in value_stack]
                    print("Stack:")
                    for p in pretty
                        print(p)
                    end
                end
            end
        end
    end
    
        
    token = if token != nothing new_borrow_pos("$END","",token) else Token("$END","",0,1,1) end
    while true
        _action, arg = get_action(token)
        if _action == Shift
            @assert arg == p.end_state
            val ,= value_stack
            return val
        else
            reduce(arg)
        end
    end
end

