"""This module implements a LALR(1) Parser
"""
# Author: Erez Shinan (2017)
# Email : erezshin@gmail.com

# Adapted to Julia by James Hester (2019)
# Email : james.r.hester@gmail.com

struct LALRParser
    _parse_table
    parser_conf
    parser
end

LALRParser(parser_conf;debug=false) = begin
    @assert all([r.options == nothing || r.options.priority == nothing for r in parser_conf.rules])
    analysis = LALR_Analyzer(parser_conf,debug=debug)
    compute_lookahead!(analysis)
    callbacks = Dict([(rule => get(parser_conf.callback,
                                 if rule.alias != nothing
                                 rule.alias
                                 else rule.origin
                                 end,
                                   nothing)
                       ) for rule in parser_conf.rules])
    parser = _LALRParser(analysis.parse_table,callbacks)
    LALRParser(analysis.parse_table,parser_conf,parser)
end

parse(l::LALRParser,args...;kwargs...) = parse(l.parser,args...;kwargs...)

struct _LALRParser
    states
    start_state
    end_state
    callbacks
end

_LALRParser(parse_table,callbacks) = begin
    _LALRParser(parse_table.states,parse_table.start_state,parse_table.end_state,callbacks)
end

# A change from Python Lark: set_state is a channel to send the new
# state to.  This helps coordination.
parse(p::_LALRParser,seq; set_state = nothing, debug=false) = begin
    i = 0
    token = nothing
#    stream = iter(seq)   ####
    states = p.states

#    println("All known keys:")
#    for k in keys(p.callbacks)
#        println("$k")
    #    end
    state_stack = Stack{Any}()
    state_stack = push!(state_stack,p.start_state)
    value_stack = Stack{Any}()

    if set_state != nothing
        set_state(p.start_state)
    end
    
    get_action(token) = begin
        state = first(state_stack)
        try
            return states[state][token.type_]
        catch e
            if e isa KeyError
                expected = [s for s in keys(states[state]) if all(x->isuppercase(x),s)]
                throw(UnexpectedToken(token, expected, state=state))  #
            else
                rethrow(e)
            end
        end
    end
    
    reduce(rule) = begin
#        println("Reducing according to $rule")
        size = length(rule.expansion)
        if size>0
            s = grab!(value_stack,size)
            for i = 1:size
                pop!(state_stack)
            end
        else
            s = []
        end
        
        value = p.callbacks[rule](s)

        _action, new_state = states[first(state_stack)][rule.origin.name]
        @assert _action == :shift
        push!(state_stack,new_state)
        push!(value_stack,value)
    end

    # Main LALR-parser loop
    for token in seq
        #println("Seen token $token, type $(token.type_)")
        while true
            action, arg = get_action(token)
            @assert arg != p.end_state
            
            if action == :shift
                if debug
                    println("Seen $(token.type_),\n shifting to state $arg")
                end
                push!(state_stack,arg)
                push!(value_stack,token)
                if set_state != nothing
                    set_state(arg)
                end
                break # next token
            else
                if debug
                    println("Seen $(token.type_),\n reducing with $arg")
                end
                reduce(arg)
                if debug
                    println("Reduced to \n $(first(state_stack))")
                    println("Stack: $value_stack")
                end
            end
        end
    end
    
        
    token = if token != nothing new_borrow_pos("\$END","",token) else Token("\$END","",pos_in_stream=1,line=1,column=1) end
    while true
        _action, arg = get_action(token)
        if _action == :shift
            @assert arg == p.end_state
            @assert length(value_stack) == 1
            return pop!(value_stack)
        else
            reduce(arg)
        end
    end
end

