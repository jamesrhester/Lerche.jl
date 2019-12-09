# A RulePtr tracks a location within a rule
#== 
 The index value has a pythonic meaning of
 being "between" two values. So an index of
 0 means 'prior to the first element'
 and an index of length = 'after the last
 element' (and therefore satisfied). So
 in Python the rule starts at zero and
 finishes at length (even though the
 expansion only goes to coordinate length-1)

 We emulate this in Julia by interpreting
 the index accordingly as zero-based. 

==#
struct RulePtr
    rule::Rule
    index::Int
    RulePtr(r::Rule,i) = begin
        @assert i <= length(r.expansion) "$i beyond end of $r"
        new(r,i)
        end
end

Base.show(io::IO,r::RulePtr) = begin
    if r.index == 0
        before = []
        after = r.rule.expansion
    elseif r.index < length(r.rule.expansion) #emulate python
        before = r.rule.expansion[1:r.index]
        after = r.rule.expansion[r.index+1:end]
    else
        before = r.rule.expansion
        after = []
    end
    print(io,"<$(r.rule.origin.name) : $(join(before," ")) * $(join(after," "))>")
end

next(r::RulePtr) = r.rule.expansion[r.index+1]

advance(r::RulePtr,sym) = begin
    @assert next(r) == sym
    RulePtr(r.rule,r.index + 1)
end

# Have we satisfied the rule by getting to the end?
is_satisfied(r::RulePtr) = r.index == length(r.rule.expansion)

Base.isequal(r1::RulePtr,r2::RulePtr) = r1.rule == r2.rule && r1.index == r2.index
Base.hash(r1::RulePtr) = hash(r1.rule)+hash(r1.index)

update_set(set1,set2) = begin
    if isempty(set2)
        return false
    end
    old_len = length(set1)
    union!(set1,set2)
    return old_len < length(set1)
end
"""Calculate FOLLOW sets.

Adapted from: http://lara.epfl.ch/w/cc09:algorithm_for_first_and_follow_sets

"""

calculate_sets(rules) = begin
    symbols = union(Set(sym for rule in rules for sym in rule.expansion), Set(rule.origin for rule in rules))
    # foreach grammar rule X ::= Y(1) ... Y(k)
    # if k=0 or {Y(1),...,Y(k)} subset of NULLABLE then
    #   NULLABLE = NULLABLE union {X}
    # for i = 1 to k
    #   if i=1 or {Y(1),...,Y(i-1)} subset of NULLABLE then
    #     FIRST(X) = FIRST(X) union FIRST(Y(i))
    #   for j = i+1 to k
    #     if i=k or {Y(i+1),...Y(k)} subset of NULLABLE then
    #       FOLLOW(Y(i)) = FOLLOW(Y(i)) union FOLLOW(X)
    #     if i+1=j or {Y(i+1),...,Y(j-1)} subset of NULLABLE then
    #       FOLLOW(Y(i)) = FOLLOW(Y(i)) union FIRST(Y(j))
    # until none of NULLABLE,FIRST,FOLLOW changed in last iteration
    NULLABLE = Set()
    FIRST = Dict()
    FOLLOW = Dict()
    for sym in symbols
        FIRST[sym] = if sym.is_term Set([sym]) else Set() end
        FOLLOW[sym] = Set()
    end

    # Calculate NULLABLE and FIRST
    changed = true
    while changed
        changed = false
        for rule in rules
            if Set(rule.expansion) <= NULLABLE
                if update_set(NULLABLE,Set([rule.origin]))
                    changed = true
                end
            end
            for (i,sym) in enumerate(rule.expansion)
                if i==1 || Set(rule.expansion[1:i-1]) <= NULLABLE
                    if update_set(FIRST[rule.origin],FIRST[sym])
                        changed = true
                    end
                end
            end
        end
    end

    # Calculate FOLLOW
    changed = true
    while changed
        changed = false

        for rule in rules
            for (i,sym) in enumerate(rule.expansion)
                if i == length(rule.expansion) || Set(rule.expansion[i+1:end]) <= NULLABLE
                    if update_set(FOLLOW[sym], FOLLOW[rule.origin])
                        changed = true
                    end
                end

                for j in i+1:length(rule.expansion)
                    if j==i+1 || Set(rule.expansion[i+1:j-1]) <= NULLABLE
                        if update_set(FOLLOW[sym], FIRST[rule.expansion[j]])
                            changed = true
                        end
                    end
                end
            end
        end
    end

    #==
    println("\nFIRST: $(length(FIRST)) items")
    for (i,j) in FIRST
        println("$i:$j")
    end
    println("\nFOLLOW: $(length(FOLLOW)) items")
    for (i,j) in FOLLOW
        println("$i:$j")
    end
    println("\nNULLABLE: $NULLABLE") ==#
    return FIRST,FOLLOW, NULLABLE
end

abstract type GrammarAnalyzer end

init_analyser!(g::GrammarAnalyzer,parser_conf;debug=false) = begin
    g.debug = debug
    rules = push!(parser_conf.rules, Rule(NonTerminal("\$root"), [NonTerminal(parser_conf.start), Terminal("\$END")]))
    g.rules_by_origin = classify(rules, key = r -> r.origin)
    @assert length(rules) == length(Set(rules))
    for r in rules
        for sym in r.expansion
            if !(sym.is_term || sym in collect(keys(g.rules_by_origin)))
                throw(GrammarError("Using an undefined rule: $sym"))
            end
        end
    end
    g.start_state = expand_rule(g,NonTerminal("\$root"))
    g.FIRST, g.FOLLOW, g.NULLABLE = calculate_sets(rules)
    return g
end

"""Returns all init_ptrs accessible by rule (recursive)"""
expand_rule(g::GrammarAnalyzer,rule) = begin
    init_ptrs = Set()
    _expand_rule(_rule) = begin
        Channel() do rule_chan
            #@assert !_rule.is_term _rule
            for r in g.rules_by_origin[_rule]
                init_ptr = RulePtr(r,0)
                push!(init_ptrs,init_ptr)

                if !isempty(r.expansion)
                    # Next symbol from this
                    new_r = next(init_ptr)
                    if !new_r.is_term
                        # Non-terminal, send it back
                        put!(rule_chan,new_r)
                    end
                end
            end
        end
    end

    _ = collect(bfs([rule], _expand_rule))

    return Set(init_ptrs)
end

_first(g::GrammarAnalyzer,r) = begin
    if r.is_term
        return Set(r)
    else
        return Set([next(rp) for rp in g.expand_rule(r) if next(rp).is_term])
    end
end
