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

ExpandSingleChild(node_builder) = function(c)
    if length(c) == 1
        return c[1]
    else
        return node_builder(c)
    end
end

PropagatePositions(node_builder) = function(children)
    res = node_builder(children)
    if res isa Tree
        meta(res).empty = true
        for c in children
            if c isa Tree && c.children != nothing && !(meta(c).empty)
                meta(res).line = meta(c).line
                meta(res).column = meta(c).column
                meta(res).start_pos = meta(c).start_pos
                meta(res).empty = false
                break
            elseif c isa Token
                meta(res).line = c.line
                meta(res).column = c.column
                meta(res).start_pos = c.pos_in_stream
                meta(res).empty = false
                break
            end
        end

        for c in reverse(children)
            if c isa Tree && c.children != nothing && !(meta(c).empty)
                meta(res).end_line = meta(c).end_line
                meta(res).end_column = meta(c).end_column
                meta(res).end_pos = meta(c).end_pos
                meta(res).empty = false
                break
            elseif c isa Token
                meta(res).end_line = c.end_line
                meta(res).end_column = c.end_column
                meta(res).end_pos = c.pos_in_stream + length(c.value)
            break
            end
        end
    end
    return res
end

ChildFilter(to_include,node_builder) = function(c)
    filtered = []
    for (i,to_expand) in to_include
        if to_expand
            push!(filtered, c[i].children)
        else
            append!(filtered,c[i])
        end
    end
    return node_builder(filtered)
end

ChildFilterLALR(to_include,node_builder) = function(c)
    filtered = []
    for (i, to_expand) in to_include
            if to_expand
                if filtered
                    push!(filtered, c[i].children)
                else   # Optimize for left-recursion
                    filtered = c[i].children
                end
            else
                append!(filtered,c[i])
            end
    end
    return self.node_builder(filtered)
end

# Reproduce the behaviour of the partial wrapper
# Probably can write a macro for this
ChildFilterLALR(to_include) = function (node_builder)
    return ChildFilterLALR(to_include,node_builder)
end

    
_should_expand(sym) = !sym.is_term && first(sym.name) == '_'

maybe_create_child_filter(expansion,keep_all_tokens) = begin
    to_include = [(i,_should_expand(sym)) for (i,sym) in enumerate(expansion)
                  if keep_all_tokens || !(sym.is_term && sym.filter_out)]
    if length(to_include) < length(expansion) || any([to_expand for (i,to_expand) in to_include])
        return ChildFilterLALR(to_include)   #This was a partial application in Python
    end
end

AmbiguousExpander(to_expand,tree_class,node_builder) = begin
    _is_ambig_tree(child) = hasproperty(child,:data) && child.data == "_ambig"
    ambiguous = [i for i in to_expand if _is_ambig_tree(children[i])]
    if !isempty(ambiguous)
        expand = map(enumerate(children)) do i,child
            if i in ambiguous child.children else repeated(child) end
        end
        return tree_class("_ambig",[node_builder(collect(f[1])) for f in product(zip(expand...))])
    end
    return node_builder(children)
end


maybe_create_ambiguous_expander(tree_class,expansion,keep_all_tokens) = begin
    to_expand = [i for (i,sym) in enumerate(expansion)
                 if keep_all_tokens || ((!(sym.is_term && sym.filter_out)) && _should_expand(sym))]
    if length(to_expand)>0
        return partial(AmbiguousExpander, to_expand, tree_class)
    end
end

abstract type Callback end

# No need for the @wrap decorator from Python as that is just
# for function naming and doc strings
ptb_inline_args(func) = begin
    return children -> func(children...)
    end
end

struct ParseTreeBuilder
    tree_class
    propagate_positions
    always_keep_all_tokens
    ambiguous
    rule_builders
    user_aliases::Dict
end

ParseTreeBuilder(rules,tree_class,propagate_positions,keep_all_tokens,ambiguous) = begin
    rule_builders = collect(_init_builders(rules,keep_all_tokens,ambiguous,propagate_positions))
    ParseTreeBuilder(tree_class,propagate_positions,keep_all_tokens,ambiguous,rule_builders,
                     Dict())
end

ParseTreeBuilder(rules,tree_class) = ParseTreeBuilder(rules,tree_class,false,false,false)

# Some other keyword combinations from Python may be needed

# The wrapper chain provides a set of operations to perform based on options, very compact

## Note the Python behaviour of 'and' and 'or'; they return the most recent argument
# that has been evaluated

_init_builders(ptb,rules,all_tokens,ambiguous,propagate_positions) = Channel() do buildchan
    for rule in rules
        options = rule.options
        keep_all_tokens = all_tokens || (if !isnothing(options) 
                                         options.keep_all_tokens
                                         else false end)
        expand_single_child = if !isnothing(options) options.expand1 else false end
        wrapper_chain =
            filter(x->x != false, [
                if propagate_positions PropagatePositions else false end,
                if (expand_single_child && isnothing(rule.alias)) ExpandSingleChild else false end,
                maybe_create_child_filter(rule.expansion, keep_all_tokens, ambiguous),
                if ptb.ambiguous
                maybe_create_ambiguous_expander(ptb.tree_class,rule.expansion,keep_all_tokens)
                else false
                end
            ])
        put!(buildchan, (rule,wrapper_chain))
    end
end

# A callback is a collection of functions indexed by internal name
# TODO replace Callback property setting with a Dictionary

create_callback(ptb::ParseTreeBuilder;transformer=nothing) = begin
    callback = Dict()
    i = 0
    for (rule,wrapper_chain) in ptb.rule_builders
        internal_callback_name = "_cb$i_$(rule.origin)"
        i+=1
        user_callback_name = if !isnothing(rule.alias) rule.alias else rule.origin.name end
        try
            f = getproperty(transformer,Symbol(user_callback_name))
            @assert !has_property(f,:meta)
            if hasproperty(f,:inline)
                f = ptb_inline_args(f)
            end
        catch           
            f = partial(ptb.tree_class,user_callback_name)  #create the parse tree
        end
        ptb.user_aliases[rule] = rule.alias
        rule.alias = internal_callback_name
        for w in wrapper_chain
            f = w(f)  #ExpandSingleChild, maybe_create_child_filter,propagate positions
        end
        if Symbol(internal_callback_name) in keys(callback)
            error("Rule '$rule' already exists")
        end
        callback[Symbol(internal_callback_name]=f
    end
    return callback
end
