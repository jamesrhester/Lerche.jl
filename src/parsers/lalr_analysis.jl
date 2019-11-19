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
    states #::Dict{Set{RulePtr},Dict{String,Tuple{Action,Union{Rule,Set{RulePtr}}}}}
    start_state
    end_state
end

# This was a class method in Python but there seem to be
# no subclasses at the moment??
from_ParseTable(parse_table) = begin
    enum = collect(keys(parse_table.states))
    state_to_idx = Dict((s=>i) for (i,s) in enumerate(enum))
    int_states = Dict()
    #==println("\nState table:")
    for s in keys(state_to_idx)
        print("\n$s")
        print("$(state_to_idx[s])")
    end
    ==#
    
    for (s,la) in parse_table.states
        la = Dict([(k=>if v[1] == Shift
                       (v[1],state_to_idx[v[2]])
                       else
                        v end) for (k,v) in la])
        int_states[state_to_idx[s] ] = la
    end

    start_state = state_to_idx[parse_table.start_state]
    end_state = state_to_idx[parse_table.end_state]
    println("Start state: $start_state")
    println("End state: $end_state")
    return ParseTable(int_states,start_state,end_state)
end

# Grammar analysis

mutable struct LALR_Analyzer <: GrammarAnalyzer
    debug
    rules_by_origin
    start_state
    end_states
    end_state
    states
    _parse_table
    parse_table
    FIRST
    FOLLOW
    NULLABLE
end

LALR_Analyzer(parser_conf;debug=false) = begin
    l = LALR_Analyzer(false,nothing,nothing,nothing,nothing,nothing,nothing,nothing,nothing,nothing,nothing)
    init_analyser!(l,parser_conf,debug=debug)
    return l
end

compute_lookahead!(l::LALR_Analyzer) = begin
    # println("Analysis before: $l")
    l.end_states = []
    l.states = Dict()
    # println("Start state: $(l.start_state)")
    step(state) = Channel() do state_chan
        # println("Analysing state: $state")
        lookahead = Dict{LarkSymbol,Array{Tuple{Action,
                                                Union{Rule,Set{RulePtr}}}}}()  #should be list if missing (Python defaultdict(list))
        sat, unsat = classify_bool(state, rp -> is_satisfied(rp))
        for rp in sat
            #println("Stepping: rp is $rp")    
            for term in get(l.FOLLOW,rp.rule.origin,())
                get!(lookahead,term,[])
                push!(lookahead[term],(Reduce,rp.rule))
            end
        end
        #println("Unsatisfied rules: $unsat")
        d = classify(unsat, key=rp -> next(rp))
        for (sym,rps) in d
            # println("Looking at $sym, Rule pointers are $rps")
            rps = Set(advance(rp,sym) for rp in rps)
            #println("One step forward...$rps")
            for rp in rps
                if !is_satisfied(rp) && !(next(rp).is_term)
                    union!(rps,expand_rule(l,next(rp)))
                    #println("Added more rules: Rule pointers now $rps")
                end
            end
            new_state = Set(rps)
            get!(lookahead,sym,[])
            # println("Adding Shift to $sym")
            push!(lookahead[sym],(Shift,new_state))
            if sym == Terminal("\$END")
                push!(l.end_states,new_state)
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

        l.states[state] = Dict((k.name=>v[1]) for (k,v) in lookahead)

    end   #of step function

    for _ in bfs([l.start_state], step)
        # do nothing; this just alters the contents of l
    end

    l.end_state = first(l.end_states)

    println("End states: $(l.end_state)")
    l._parse_table = ParseTable(l.states,l.start_state,l.end_state)

    #println("Analyser info: $l")
    if l.debug
        l.parse_table = l._parse_table
    else
        l.parse_table = from_ParseTable(l._parse_table)
    end
end

                     
            
    


