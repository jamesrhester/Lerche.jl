"""This module builds a LALR(1) transition-table for lalr_parser.jl

For now, shift/reduce conflicts are automatically resolved as shifts.
"""

# Author: Erez Shinan (2017)
# Email : erezshin@gmail.com

# Adapted from Python by James Hester (2019)
# Email: james.r.hester@gmail.com

struct Action
    name::String
end

Shift = Action("Shift")
Reduce = Action("Reduce")

struct ParseTable
    states
    start_state
    end_state
end

# This was a class method in Python but there seem to be
# no subclasses at the moment??
from_ParseTable(parse_table) = begin
    enum = collect(parse_table.states)
    state_to_idx = Dict((s=>i) for (i,s) in enumerate(enum))
    int_states = Dict()

    for (s,la) in parse_table.states
        la = Dict([(k=>(v[1],
                       if v[0] == Shift
                       state_to_idx[v[2]]
                       else
                        v)) for (k,v) in la])
        int_states[state_to_idx[s] ] = la
    end

    start_state = state_to_idx[parse_table.start_state]
    end_state = state_to_idx[parse_table.end_state]
    ParseTable(int_states,start_state,end_state)
end

# Grammar analysis

mutable struct LALR_Analyzer <: GrammarAnalyzer
    debug
    rules_by_origin
    start_state
    end_states
    states
    _parse_table
    parse_table
    FIRST
    FOLLOW
    NULLABLE
    LALR_analyzer(parser_conf;debug=false) = init_analyser!(new(),parser_conf,debug=debug)
end

compute_lookahead(l::LALR_Analyzer) = begin
    l.end_states = []
    l.states = Dict()
    step(state) = begin
        Channel() do state_chan
            lookahead = Dict()  #should be list if missing
            sat, unsat = classify_bool(state, rp -> rp.is_satisfied)
            for rp in sat
                for term in l.FOLLOW.get(rp.rule.origin,())
                    get!(lookahead,term,[])
                    append!(lookahead[term],(Reduce,rp.rule))
                end
            end
            d = classify(unsat, rp -> rp.next)
            for (sym,rps) in d
                rps = Dict(rp.advance(sym) for rp in rps)
                for rp in Set(rps)
                    if !rp.is_satisifed && !rp.next.is_term
                        union!(rps,l.expand_rule(rp.next))
                    end
                end
                new_state = Set(rps)
                get!(lookahead,sym,[])
                append!(lookahead[sym],(Shift,new_state))
                if sym == Terminal("$END")
                    append!(l.end_states,new_state)
                end
                put!(state_chan,new_state)
            end

            for (k,v) in lookahead
                if length(v) > 1
                    if l.debug
                        println("WARNING: Shift/reduce conflict for $(k.name): $v. Resolving as shift")
                    end
                    for x in v
                        if x[1] == Shift
                            lookahead[k] = [x]
                        end
                    end
                end
            end

            for (k,v) in lookahead
                if !(length(v) == 1)
                    throw(GrammarError("Collision in $k: $(join(", ",["\n * $x" for x in v]))"))
                end
            end

            l.states[state] = Dict(k.name=>v[1] for (k,v) in lookahead)

        end   #of step function

        for _ in bfs([l.start_state], step)
            # do nothing; this just alters the contents of l
        end

        l.end_state , = l.end_states

        l._parse_table = ParseTable(l.states,l.start_state,l.end_state)

        if l.debug
            l.parse_table = l._parse_table
        else
            l.parse_table = from_ParseTable(l._parse_table)
        end
    end
end

                     
            
    


