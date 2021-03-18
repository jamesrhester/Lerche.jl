"""This module builds a LALR(1) transition-table for lalr_parser.jl

For now, shift/reduce conflicts are automatically resolved as shifts.
"""

# Author: Erez Shinan (2017)
# Email : erezshin@gmail.com

# Adapted from Python by James Hester (2019)
# Email: james.r.hester@gmail.com

#==
struct Action
    name::Symbol
end
abstract type Action end
struct Shift <: Action end
struct Reduce <: Action end

#const Shift = Action("Shift")
#const Reduce = Action("Reduce")
==#

struct ParseTable
    states #::Dict{Set{RulePtr},Dict{String,Tuple{Symbol,Union{Rule,Set{RulePtr}}}}}
    start_states
    end_states
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
        la = Dict([(k=>if v[1] == :shift
                       (v[1],state_to_idx[v[2]])
                       else
                        v end) for (k,v) in la])
        int_states[state_to_idx[s] ] = la
    end

    start_states = Dict([start=>state_to_idx[s] for (start,s) in parse_table.start_states])
    end_states = Dict([start=>state_to_idx[s] for (start,s) in parse_table.end_states])
    return ParseTable(int_states,start_states,end_states)
end

# digraph and traverse, see The Theory and Practice of Compiler Writing

# computes F(x) = G(x) union (union { G(y) | x R y })
# X: nodes
# R: relation (function mapping node -> list of nodes that satisfy the relation)
# G: set valued function
digraph(X, R, G) = begin
    F = Dict()
    S = []
    N = Dict{Any,Int64}()
    for x in X
        N[x] = 0
    end
    for x in X
        # this is always true for the first iteration, but N[x] may be updated in traverse below
        if N[x] == 0
            traverse(x, S, N, X, R, G, F)
        end
    end
    return F
end

# x: single node
# S: stack
# N: weights
# X: nodes
# R: relation (see above)
# G: set valued function
# F: set valued function we are computing (map of input -> output)
traverse(x, S, N, X, R, G, F) = begin
    push!(S,x)
    d = length(S)
    N[x] = d
    F[x] = G[x]
    for y in R[x]
        if N[y] == 0
            traverse(y, S, N, X, R, G, F)
        end
        n_x = N[x]
        @assert n_x > 0
        n_y = N[y]
        @assert n_y != 0
        if (n_y > 0) && (n_y < n_x)
            N[x] = n_y
        end
        union!(F[x],F[y])
    end
    if N[x] == d
        f_x = F[x]
        while true
            z = pop!(S)
            N[z] = -1
            F[z] = f_x
            if z == x
                break
            end
        end
    end
end

# Grammar analysis

mutable struct LALR_Analyzer <: GrammarAnalyzer
    debug
    rules_by_origin
    start_states
    end_states
    parse_table
    FIRST
    FOLLOW
    NULLABLE
    nonterminal_transitions
    directly_reads::DefaultDict
    reads::DefaultDict
    includes::DefaultDict
    lookback::DefaultDict
    lr0_states::Set
    lr0_start_states::Dict
    lr0_rules_by_origin
end

LALR_Analyzer(parser_conf;debug=true) = begin
    l = LALR_Analyzer(debug,nothing,nothing,nothing,nothing,nothing,nothing,nothing,[],DefaultDict{Any,Set}(),DefaultDict{Any,Set}(),DefaultDict{Any,Set}(),DefaultDict{Any,Set}(),Set(),Dict(),[])
    init_analyser!(l,parser_conf,debug=debug)
    return l
end

compute_lr0_states(l::LALR_Analyzer) = begin
    l.lr0_states = Set()
    cache = Dict()
    println("lr0 start states: $(l.lr0_start_states)")
    step(state) = Channel() do state_chan
        _, unsat = classify_bool(state.closure,rp->is_satisfied(rp))
        d = classify(unsat,key=rp->next(rp))
        for (sym,rps) in d
            kernel = Set([advance(rp,sym) for rp in rps])
            new_state = get(cache,kernel,nothing)
            if new_state === nothing
                closure = copy(kernel)
                for rp in kernel
                    if !is_satisfied(rp) && !is_terminal(next(rp))
                        union!(closure,expand_rule(l,next(rp),l.lr0_rules_by_origin))
                    end
                end
                new_state = LR0ItemSet(kernel,closure)
                cache[kernel] = new_state
            end
            state.transitions[sym] = new_state
            put!(state_chan,new_state)
        end
        push!(l.lr0_states,state)
    end
    collect(bfs(values(l.lr0_start_states),step))
    #println("lr0_states after: $(l.lr0_states)")
    println("lr0_states: $(length(l.lr0_states))")
    println("lr0_rules_by_origin: $(length(l.lr0_rules_by_origin))")
end

compute_reads_relations(l::LALR_Analyzer) = begin
    # handle start state
    for root in values(l.lr0_start_states)
        @assert length(root.kernel) == 1
        for rp in root.kernel
            @assert rp.index == 0
            l.directly_reads[(root, next(rp))] = Set([ Terminal("\$END") ])
        end
    end
    for state in l.lr0_states
        seen = Set()
        for rp in state.closure
            if is_satisfied(rp)
                continue
            end
            s = next(rp)
            # if s is a not a nonterminal
            if !(s in keys(l.lr0_rules_by_origin))
                continue
            end
            if s in seen
                continue
            end
            push!(seen,s)
            nt = (state, s)
            push!(l.nonterminal_transitions,nt)
            dr = l.directly_reads[nt]
            r = l.reads[nt]
            next_state = state.transitions[s]
            for rp2 in next_state.closure
                if is_satisfied(rp2) continue end
                s2 = next(rp2)
                # if s2 is a terminal
                if !(s2 in keys(l.lr0_rules_by_origin))
                    push!(dr,s2)
                end
                if s2 in l.NULLABLE
                    push!(r,(next_state, s2))
                end
            end
        end
    end
    #println("Nonterminal transitions: $(length(l.nonterminal_transitions))")
    #__s = sort(l.nonterminal_transitions)
    #for (i,s1) in enumerate(__s) println("$(i-1): $s1") end
end

compute_includes_lookback(l::LALR_Analyzer) = begin
    for nt in l.nonterminal_transitions
        state, nonterminal = nt
        #println("\n===\nState $state\n")
        includes = []
        lookback = l.lookback[nt]
        for rp in state.closure
            if rp.rule.origin != nonterminal
                continue
            end
            #println("Rule $rp")
            # traverse the states for rp(.rule)
            state2 = state
            for i in rp.index:(length(rp.rule.expansion)-1)
                s = rp.rule.expansion[i+1]
                nt2 = (state2, s)
                state2 = state2.transitions[s]
                if !(nt2 in keys(l.reads))
                    continue
                end
                j = i+1  #to fix the scope so we can mimic Python for...else
                for outer j in (i + 1):(length(rp.rule.expansion)-1)
                    if !(rp.rule.expansion[j+1] in l.NULLABLE)
                        break
                    end
                end
                # mimic for...else in Python code
                if j == length(rp.rule.expansion)
                    push!(includes,nt2)
                end
            end
            # state2 is at the final state for rp.rule
            if rp.index == 0
                for rp2 in state2.closure
                    if (rp2.rule == rp.rule) && is_satisfied(rp2)
                        push!(lookback,(state2, rp2.rule))
                    end
                end
            end
        end
        
        for nt2 in includes
            push!(l.includes[nt2],nt)
        end
    end
end

compute_lookaheads(l::LALR_Analyzer) = begin
    #println("$(length(l.nonterminal_transitions)), $(length(l.reads)), $(length(l.directly_reads))")
    read_sets = digraph(l.nonterminal_transitions, l.reads, l.directly_reads)
    #println("Includes: length $(length(l.includes))")
    follow_sets = digraph(l.nonterminal_transitions, l.includes, read_sets)
    
    for (nt, lookbacks) in l.lookback
        #println("State $nt: lookaheads $(length(follow_sets[nt]))")
        for (state, rule) in lookbacks
            for s in follow_sets[nt]
                push!(state.lookaheads[s],rule)
            end
        end
    end
end

compute_lalr1_states(l::LALR_Analyzer) = begin
    m = Dict()
    reduce_reduce = []
    for state in l.lr0_states
        actions = Dict()
        for (la, next_state) in state.transitions
            actions[la] = (:shift, next_state.closure)
        end
        
        for (la, rules) in state.lookaheads
            if length(rules) > 1
                # Try to resolve conflict based on priority
                p = [(r.options.priority || 0, r) for r in rules]
                sort!(p,by=r -> r[1], rev=true)
                best, second_best = p[1:2]
                if best[1] > second_best[1]
                    rules = [best[2]]
                else
                    push!(reduce_reduce,(state, la, rules))
                end
            end
            if la in keys(actions)
                if l.debug
                    @warn "Shift/Reduce conflict for terminal $(la.name): (resolving as shift)"
                    @warn " * $(first(rules))"
                end
            else
                actions[la] = (:reduce, first(rules))
            end
        end
        
        m[state] = Dict([k.name => v for (k, v) in actions])
    end
    
    if !isempty(reduce_reduce)
        msgs = []
        for (state, la, rules) in reduce_reduce
            msg = "Reduce/Reduce collision in $la between the following rules: "*join([ "\n\t- $r"  for r in rules ])
            if l.debug
                msg *= "\n    collision occurred in state: {" * join(["\n\t$x" for x in state.closure]) * "\n    }"
            end
            push!(msgs,msg)
        end    
        throw(GrammarError(join(msgs,"\n\n")))
    end

    states = Dict([k.closure => v for (k, v) in m ])

    # compute end states
    end_states = Dict()
    for state in keys(states)
        for rp in state
            for start in keys(l.lr0_start_states)
                if rp.rule.origin.name == ("\$root_" * start) && is_satisfied(rp)
                    @assert !(start in keys(end_states))
                    end_states[start] = state
                end
            end
        end
    end
        
    _parse_table = ParseTable(states, Dict([start=> state.closure for (start, state) in l.lr0_start_states]), end_states)

    if l.debug
        l.parse_table = _parse_table
    else
        println("Integer states")
        l.parse_table = from_ParseTable(_parse_table)
    end
end

compute_lalr!(l::LALR_Analyzer) = begin
    compute_lr0_states(l)
    compute_reads_relations(l)
    compute_includes_lookback(l)
    compute_lookaheads(l)
    compute_lalr1_states(l)
end

            
    


