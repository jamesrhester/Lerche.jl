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

ChildFilter(to_include,append_none,node_builder) = function(c)
    filtered = Union{Token,Tree,Nothing}[]
    for (i,to_expand,add_none) in to_include
        if add_none > 0
            append!(filtered,fill(nothing,add_none))
        end
        if to_expand
            append!(filtered,c[i].children)
        else
            push!(filtered,c[i])
        end
    end

    if append_none > 0
        append!(filtered,fill(nothing,append_none))
    end
    node_builder(filtered)
end

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

ChildFilterLALR_NoPlaceholders(to_include,node_builder) = function(c)
    filtered = Union{Token,Tree}[]
    for (i, to_expand) in to_include
        if to_expand
            if length(filtered) > 0
                append!(filtered,c[i].children)
            else
                filtered = c[i].children
            end
        else
            push!(filtered,c[i])
        end
    end
    return node_builder(filtered)
end

_should_expand(sym) = !is_terminal(sym) && first(sym.name) == '_'

# TODO: possible bug from 0-indexing in python going to 1-indexing in Julia
maybe_create_child_filter(expansion,keep_all_tokens,ambiguous,_empty_indices) = begin
    if _empty_indices !== nothing
        @assert count(x->!x,_empty_indices) == length(expansion)
        s = join([x-> x ? "1" : "0" , _empty_indices],"")
        empty_indices = [length(ones) for ones in split(s,"0")]
        @assert length(empty_indices) == length(expansion) + 1
    else
        empty_indices = fill(0,length(expansion)+1)
    end
    to_include = []
    nones_to_add = 0
    for (i,sym) in enumerate(expansion)
        nones_to_add += empty_indices[i]
        if keep_all_tokens || !(is_terminal(sym) && sym.filter_out)
            push!(to_include, (i,_should_expand(sym), nones_to_add))
            nones_to_add = 0
        end
    end
    nones_to_add += empty_indices[length(expansion)+1]

    if _empty_indices !== nothing || length(to_include) < length(expansion) || any([to_expand for (i,to_expand) in to_include])
        if _empty_indices !== nothing || ambiguous
            return partial(ambiguous ? ChildFilter : ChildFilterLALR, to_include, nones_to_add)
        else
            return partial(ChildFilterLALR_NoPlaceholders, [(i,x) for (i,x,_) in to_include])
        end
    end
    return false
end

AmbiguousExpander(to_expand,node_builder) = function(children)
    _is_ambig_tree(t) = hasproperty(t,:data) && t.data == "_ambig"
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

"""
    Propagate ambiguous intermediate nodes and their derivations up to the
    current rule.

    In general, converts

    rule
      _iambig
        _inter
          someChildren1
          ...
        _inter
          someChildren2
          ...
      someChildren3
      ...

    to

    _ambig
      rule
        someChildren1
        ...
        someChildren3
        ...
      rule
        someChildren2
        ...
        someChildren3
        ...
      rule
        childrenFromNestedIambigs
        ...
        someChildren3
        ...
      ...

    propagating up any nested '_iambig' nodes along the way.
    """
AmbiguousIntermediateExpander(node_builder,children) = begin
    _is_iambig_tree(child) = begin
        return hasproperty(child, :data) && child.data == "_iambig"
    end
    
    _collapse_iambig(children) = begin
        #==
            Recursively flatten the derivations of the parent of an '_iambig'
            node. Returns a list of '_inter' nodes guaranteed not
            to contain any nested '_iambig' nodes, or None if children does
            not contain an '_iambig' node.
        ==#

        # Due to the structure of the SPPF,
        # an '_iambig' node can only appear as the first child
        if !isempty(children) && _is_iambig_tree(children[1])
            iambig_node = children[1]
            result = []
            for grandchild in iambig_node.children
                collapsed = _collapse_iambig(grandchild.children)
                if !isempty(collapsed)
                    for child in collapsed
                        append!(child.children, children[2:end])
                        append!(result, collapsed)
                    end
                else
                    new_tree = Tree("_inter", append!(copy(grandchild.children), children[2:end]))
                    push!(result,new_tree)
                end
            end
            return result
        end
    end
    
    collapsed = _collapse_iambig(children)
    if !isempty(collapsed)
        processed_nodes = [node_builder(c.children) for c in collapsed]
        return Tree("_ambig", processed_nodes)
    end
    return node_builder(children)
end

abstract type Callback end

# No need for the @wrap decorator from Python as that is just
# for function naming and doc strings
ptb_inline_args(func) = begin
    return children -> func(children...)
    end

inplace_transformer(func) = begin
    return children -> func(Tree(Symbol(func),children))
end

apply_visit_wrapper(func,name,wrapper) = begin
    return children -> wrapper(func,name,children,nothing)
end

struct ParseTreeBuilder
    rule_builders::Array{Tuple{Rule,Array{Function}},1}
end

ParseTreeBuilder(rules::Array{Rule,1};propagate_positions=false,ambiguous=false,maybe_placeholders=false)= begin
    rule_builders = _init_builders(rules,ambiguous,propagate_positions,maybe_placeholders)
    ParseTreeBuilder(rule_builders)
end

# The wrapper chain provides a set of operations to perform based on options, very compact

## Note the Python behaviour of 'and' and 'or'; they return the most recent argument
# that has been evaluated

_init_builders(rules,ambiguous,propagate_positions,maybe_placeholders) = begin
    build_list = []
    for rule in rules
        options = rule.options
        keep_all_tokens = options !== nothing ? options.keep_all_tokens : false
        expand_single_child = options.expand1
        wrapper_chain =
            filter(x-> typeof(x) != Bool, [
                if (expand_single_child && rule.alias == nothing) ExpandSingleChild else false end,
                maybe_create_child_filter(rule.expansion, keep_all_tokens, ambiguous, maybe_placeholders ? options.empty_indices : nothing),
                if propagate_positions PropagatePositions else false end,
                if ambiguous
                maybe_create_ambiguous_expander(rule.expansion,keep_all_tokens)
                else false
                end,
                if ambiguous AmbiguousIntermediateExpander else false end
            ])
        push!(build_list, (rule,wrapper_chain))
    end
    return build_list
end

# These callbacks are executed during parsing, as each rule is
# matched (i.e. the reduction step). If a transformer has been
# specified, the appropriate method is called; by default a
# parse tree is constructed using Tree, which can later be
# visited by transformers or visitors.

# Note the use of partial to store partial information about
# the appropriate transformer to call, for later application.
# We have to supply a dummy "nothing" argument for meta.

# TODO: integrate transformer callbacks into the callback chain
# while retaining type stability. Probably requires transformers
# to be user-typed

create_callback(ptb::ParseTreeBuilder;transformer=nothing) = begin
    callbacks = Dict()
    i = 0
    for (rule,wrapper_chain) in ptb.rule_builders
        #println("Rule: $(rule.origin)")
        # Now find the actual transformer function ...
        user_callback_name =
            if rule.alias !== nothing rule.alias
            elseif rule.options.template_source !== nothing rule.options.template_source
            else rule.origin.name
            end
        f = partial(transformer_func,transformer,Val{Symbol(user_callback_name)}(),Meta())
        if typeof(transformer) <: Transformer_InPlace
            f = inplace_transformer(f)
        end
        
        for w in wrapper_chain
            if w == nothing
                println("Rule $rule has a nothing in the wrapper!!")
                @assert w!=nothing
            end
            f = w(f)  #ExpandSingleChild, maybe_create_child_filter,propagate positions
        end
        if rule in keys(callbacks)
            throw(GrammarError("Rule '$rule' already exists"))
        end
        callbacks[rule]=f
    end
    return callbacks
end
