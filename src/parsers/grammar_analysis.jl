struct RulePtr
    rule::Rule
    index::Int
end

Base.show(io::IOStream,r::RulePtr) = begin
    before = r.rule.expansion[1:r.index]
    after = r.rule.expansion[r.index:end]
    print(io,"<$(r.origin) : $(join(" ",before)) * $(join(" ",after))>")
end

next(r::RulePtr) = r.rule.expansion[r.index]

advance(r::RulePtr,sym) = begin
    @assert next(r) == sym
    RulePtr(r.rule,r.index + 1)
end

is_satisfied(r::RulePtr) = r.index == length(r.rule.expansion)

update_set(set1,set2) = begin
    if isempty(set2)
        return false
    end
    copy = Set(set1)
    union!(set1,set2)
    return set1 != copy
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
                if update_set(NULLABLE,Set(rule.origin))
                    changed = true
                end
            end
            # TODO mimic exact list behaviour of Python
            # [:i] will give empty list for i = 0
            # but [1:1] will give first element for Julia
            for (i,sym) in enumerate(rule.expansion)
                if Set(rule.expansion[1:i]) <= NULLABLE
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
                    if Set(rule.expansion[i+1:j]) <= NULLABLE
                        if update_set(FOLLOW[sym], FIRST[rule.expansion[j]])
                            changed = true
                        end
                    end
                end
            end
        end
    end

    return FIRST,FOLLOW, NULLABLE
end

abstract type GrammarAnalyzer end

init_analyser!(g::GrammarAnalyzer,parser_conf;debug=false) = begin
    g.debug = debug
    rules = append(parser_conf["rules"], [Rule(NonTerminal("$root"), [NonTerminal(parser_conf["start"]), Terminal("$END")])])
    g.rules_by_origin = classify(rules, r -> r.origin)
    @assert length(rules) == length(Set(rules))
    for r in rules
        for sym in r.expansion
            if !(sym.is_term || sym in g.rules_by_origin)
                throw(GrammarError("Using an undefined rule: $sym"))
            end
        end
    end
    g.start_state = g.expand_rule(NonTerminal("$root"))
    g.FIRST, g.FOLLOW, g.NULLABLE = calculate_sets(rules)
    return g
end

"""Returns all init_ptrs accessible by rule (recursive)"""
expand_rule(g::GrammarAnalyzer,rule) = begin
    init_ptrs = Set()
    _expand_rule(rule) = begin
        Channel() do rule_chan
            @assert !rule.is_term, rule
            for r in g.rules_by_origin[rule]
                init_ptr = RulePtr(r,1)
                push!(init_ptrs,init_ptr)

                if r.expansion != nothing
                    new_r = next(init_ptr)
                    if !new_r.is_term
                        put!(rule_chan,new_r)
                    end
                end
            end
        end
    end

    for _ in bfs([rule], _expand_rule)
    end

    return Set(init_ptrs)
end

_first(g::GrammarAnalyzer,r) = begin
    if r.is_term
        return Set(r)
    else
        return Set([next(rp) for rp in g.expand_rule(r) if next(rp).is_term])
    end
end
