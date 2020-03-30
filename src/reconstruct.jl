is_discarded_terminal(t) = is_terminal(t) && t.filter_out != nothing

struct WriteTokensTransformer <: Transformer_InPlace
    tokens
end

__default__(wtt::WriteTokensTransformer,data,children,meta) = begin
    if !hasproperty(meta,:match_tree) || meta.match_tree == false
        return Tree(data,children)
    end
    iter_args = iterate(children)
    to_write = []
    for sym in meta.orig_expansion
        if is_discarded_terminal(sym)
            t = wtt.tokens[sym.name]
            @assert (t.pattern isa PatternStr)
            push!(to_write,t.pattern.value)
        else
            x,state = iter_args
            if x isa Array
                append!(to_write,x)
            else
                if x isa Token
                    @assert (Terminal(x.type_) == sym) x
                else
                    @assert (NonTerminal(x.data) == sym) (sym,x)
                end
                push!(to_write,x)
            end
            iter_args = iterate(children,state)
        end
    end

    @assert (iter_args == nothing)
    return to_write
end

# create a function

MakeMatchTree(name,expansion) = args -> begin
        t = Tree(name,args)
        t.meta.match_tree = true
        t.meta.orig_expansion = expansion
        return t
end

struct Reconstructor
    write_tokens
    rules
    Reconstructor(write_tokens,rules) = begin
        x = new()
        w = WriteTokensTransformer(Dict(t.name=>t for t in tokens)),
        r = collect(_build_recons_rules(rules))
        x.write_tokens = w
        x.rules = r
        return x
    end
end

_build_recons_rules(rules) = Channel() do rule_chan
    expand1s = Set(r.origin for r in rules if r.options != nothing && r.options.expand1)
    aliases = Dict()
    for r in rules
        if r.alias != nothing
            current = get!(aliases,r.origin,[])
            push!(current,r.alias)
        end
    end

    rule_names = Set(r.origin for r in rules)
    nonterminals = Set(sym for sym in rule_names
                       if first(sym.name)=='_' || sym in expand1s || sym in aliases)
    for r in rules
        recons_exp = map(r.expansion) do sym
            if sym in nonterminals sym else Terminal(sym.name) end
            recons_exp = filter(sym -> !is_discarded_terminal(sym),recons_exp)
            
            # Skip self-recursive constructs
            if recons_exp == [r.origin]
                continue
            end
            sym = if r.alias NonTerminal(r.alias) else r.origin end

            put!(rule_chan, Rule(sym, recons_exp, MakeMatchTree(sym.name, r.expansion)))
        end

        for origin, rule_aliases in aliases.items()
            for alias in rule_aliases
                put!(rule_chan, Rule(origin, [Terminal(alias)], MakeMatchTree(origin.name, [NonTerminal(alias)])))
            end
            
            put!(rule_chan,Rule(origin, [Terminal(origin.name)], MakeMatchTree(origin.name, [origin])))
        end
    end
end

_match(r::Reconstructor, term, token) = begin
    if token isa Tree
        return Terminal(token.data) == term
    elseif token isa Token
        return term == Terminal(token.type_)
    end
    @assert false
end

# Not implemented (Earley parser stuff)
_reconstruct(r::Reconstructor,tree) = nothing

