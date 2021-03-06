# Does this return a function? I hope so

#== Lark architecture (Python)

For each grammar rule, Lark creates a chain of 
transformations associated with that rule. These are kept in a Callback
object, with each rule indexed by an internal name and
returning the combined function object. In order to
make it possible to configure, but not execute, the functions,
they are created as callable objects which are initialised
during callback creation.


==#

ExpandSingleChild(node_builder::Function) = function(c)
    if length(c) == 1
        return c[1]
    else
        return node_builder(c)
    end
end

PropagatePositions(node_builder::Function) = function(children)
    res = node_builder(children)
    #println("Res is $res")
    if res isa Tree
        meta(res).empty = true
        for c in children
            #println("Child: $c")
            if c isa Tree && c.children != [] && !(meta(c).empty)
                #println("Setting values from $c: $(meta(c))")
                meta(res).line = meta(c).line
                meta(res).column = meta(c).column
                meta(res).start_pos = meta(c).start_pos
                meta(res).empty = false
                break
            elseif c isa Token
                #println("Setting token values from $c")
                meta(res).line = c.line
                meta(res).column = c.column
                meta(res).start_pos = c.pos_in_stream
                meta(res).empty = false
                break
            end
        end

        for c in reverse(children)
            if c isa Tree && c.children != [] && !(meta(c).empty)
                #println("Setting end values from $c to $(meta(c))")
                meta(res).end_line = meta(c).end_line
                meta(res).end_column = meta(c).end_column
                meta(res).end_pos = meta(c).end_pos
                meta(res).empty = false
                break
            elseif c isa Token
                #println("Setting token end values from $c")
                meta(res).end_line = c.end_line
                meta(res).end_column = c.end_column
                meta(res).end_pos = c.pos_in_stream + length(c.value)
            break
            end
        end
    end
    return res
end

# Note: we explicitly give the type of `filtered`, otherwise
# it will adopt the type of c[i].children, which may be
# a subarray and therefore not allow later push! operations.

ChildFilterLALR(to_include,node_builder) = function(c)
    #println("Filtering children: $to_include")
    filtered = Union{Token,Tree}[]
    for (i, to_expand) in to_include
            if to_expand
                append!(filtered, c[i].children)
            else
                push!(filtered,c[i])
            end
    end
    return node_builder(filtered)
end
    
_should_expand(sym) = !is_terminal(sym) && first(sym.name) == '_'

maybe_create_child_filter(expansion,keep_all_tokens) = begin
    to_include = [(i,_should_expand(sym)) for (i,sym) in enumerate(expansion)
                  if keep_all_tokens || !(is_terminal(sym) && sym.filter_out)]
    if length(to_include) < length(expansion) || any([to_expand for (i,to_expand) in to_include])
        return partial(ChildFilterLALR,to_include)
    else
        false
    end
end

AmbiguousExpander(to_expand,node_builder) = function(children)
    _is_ambig_tree(child) = hasproperty(child,:data) && child.data == "_ambig"
    ambiguous = [i for i in to_expand if _is_ambig_tree(children[i])]
    if !isempty(ambiguous)
        expand = map(enumerate(children)) do (i,child)
            if i in ambiguous child.children else repeated(child) end
        end
        return Tree("_ambig",[node_builder(collect(f[1])) for f in product(zip(expand...))])
    end
    return node_builder(children)
end


maybe_create_ambiguous_expander(expansion,keep_all_tokens) = begin
    to_expand = [i for (i,sym) in enumerate(expansion)
                 if keep_all_tokens || ((!(is_terminal(sym) && sym.filter_out)) && _should_expand(sym))]
    if length(to_expand)>0
        return partial(AmbiguousExpander, to_expand)
    else
        false
    end
end

abstract type Callback end

# No need for the @wrap decorator from Python as that is just
# for function naming and doc strings
ptb_inline_args(func) = begin
    return children -> func(children...)
    end

struct ParseTreeBuilder
    propagate_positions::Bool
    always_keep_all_tokens::Bool
    ambiguous::Bool
    rule_builders::Array{Tuple{Rule,Array{Function}},1}
end

ParseTreeBuilder(rules;propagate_positions=false,keep_all_tokens=false,ambiguous=false)= begin
    rule_builders = collect(_init_builders(rules,keep_all_tokens,ambiguous,propagate_positions))
    ParseTreeBuilder(propagate_positions,keep_all_tokens,ambiguous,rule_builders)
end

# The wrapper chain provides a set of operations to perform based on options, very compact

## Note the Python behaviour of 'and' and 'or'; they return the most recent argument
# that has been evaluated

_init_builders(rules,all_tokens,ambiguous,propagate_positions) = Channel() do buildchan
    for rule in rules
        options = rule.options
        keep_all_tokens = all_tokens || (if options != nothing 
                                         options.keep_all_tokens
                                         else false end)
        expand_single_child = if options != nothing options.expand1 else false end
        wrapper_chain =
            filter(x-> typeof(x) != Bool, [
                if propagate_positions PropagatePositions else false end,
                if (expand_single_child && rule.alias == nothing) ExpandSingleChild else false end,
                maybe_create_child_filter(rule.expansion, keep_all_tokens),
                if ambiguous
                maybe_create_ambiguous_expander(rule.expansion,keep_all_tokens)
                else false
                end
            ])
        put!(buildchan, (rule,wrapper_chain))
    end
end

# These callbacks are executed during parsing, as each rule is
# matched (i.e. the reduction step). If a transformer has been
# specified, the appropriate method is called; by default a
# parse tree is constructed using Tree, which can later be
# visited by transformers or visitors.

# Note the use of partial to store partial information about
# the appropriate transformer to call, for later application.
# We have to supply a dummy "nothing" argument for meta.

create_callback(ptb::ParseTreeBuilder;transformer=nothing) = begin
    callback = Dict()
    i = 0
    for (rule,wrapper_chain) in ptb.rule_builders
        #println("Rule: $(rule.origin)")
        internal_callback_name = "_cb$(i)_$(rule.origin)"
        i = i+1
        # Now find the actual transformer function ...
        user_callback_name = if rule.alias != nothing rule.alias else rule.origin.name end
        f = partial(transformer_func,transformer,Val{Symbol(user_callback_name)}(),Meta())
        rule.alias = internal_callback_name  #remember how to call it
        for w in wrapper_chain
            if w == nothing
                println("Rule $rule has a nothing in the wrapper!!")
                @assert w!=nothing
            end
            f = w(f)  #ExpandSingleChild, maybe_create_child_filter,propagate positions
        end
        if internal_callback_name in collect(keys(callback))
            error("Rule '$rule' already exists")
        end
        callback[internal_callback_name]=f
    end
    return callback
end
