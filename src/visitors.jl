
"""
Transformers visit each node of the tree, and run the appropriate
method on it according to the node's data.

The methods (provided by the user) are dispatched according to the
type and the node's grammar rule. The returned value replaces the old
one in the structure.

They work bottom-up (or depth-first), starting with the leaves and
ending at the root of the tree.  Transformers can be used to
implement map - reduce patterns. Because nodes are reduced from
leaf to root, at any point the callbacks may assume the children
have already been transformed (if applicable).

`Transformer` can do anything `Visitor` can do, but because it
reconstructs the tree, it is slightly less efficient.

NOTE: A transformer without methods essentially performs a non-memoized deepcopy.
"""
abstract type Transformer end

"""
    visit_tokens(t::Transformer)

Should the transformer visit tokens in addition to rules.  Setting
this to false is an order of magnitude faster. For consistency with
Lark Defaults to `true`.  (For processing ignored tokens, use the
``lexer_callbacks`` options)
"""
visit_tokens(t::Transformer) = true

#== Python decorator 

The _apply_decorator Transformer classmethod finds all
members of the subclass and submits them to the supplied
decorator function (with arguments as necessary), replacing
the previous value.

The main decorator is _visitor_args_func_dec, which
takes the function/method to be decorated together with
options.  This uses the utility smart_decorator function,
which calls the supplied "stupid" decorator in the 
correct way depending on whether the argument is a
function, method, builtin function, partial application
or callable, by using the "with_self" argument to
the internal create_decorator function.

_visitor_args_func_dec seems to simply remove "self"
from the argument list if the with_self argument is
supplied, and adds the inline/meta/whole_tree arguments
to the function attributes. It appears that only
"inline" is used. 

So: if a Transformer subclass is decorated with @inline_args,
every method not beginning with "_" and belonging to the
subclass has attribute 'inline' added to it as a True/False
value. Then, when creating a callback during parse tree construction,
if inline is 'False', then the user-written function is
splatted over the children. If 'inline' is True, then the
method is assumed to be written assuming that a list of children
is one of the arguments and no splatting is done.

As an example, EBNF_TO_BNF has inline declared as True. This
means that any of the internal methods are written assuming a
list of children is supplied.

We can duplicate this with macros, if we want.
==#

"""
    Transformer_InPlace

Non-recursive `Transformer`. Changes the tree in-place instead of returning new instances
"""
abstract type Transformer_InPlace <: Transformer end

"""
    Transformer_InPlaceRecursive

Recursive. Changes the tree in-place instead of returning new instances.
"""
abstract type Transformer_InPlaceRecursive <: Transformer end

"""
    transformer_func(t::Transformer,::Val{production},meta,args)

`transformer_func` transforms `production` with children `args`. 
Methods of this function are defined using the @rule and @inline_rule 
macros and chosen by Julia method dispatch. `meta` is only used by the 
default `transformer_func`.
"""
function transformer_func end

"""
    transformer_func(t::Transformer,::Val{N},meta,children) where N
    
Default `transformer_func`
"""
transformer_func(t::Transformer,::Val{N},meta::Meta,children) where N = begin
    Tree(String(N),children,meta)
end

transformer_func(::Nothing,::Val{N},::Meta,children) where N = Tree(String(N),children)

"""
A token transformer function, works like the tree transformer
"""
function token_func end

# default
token_func(t::Transformer,::Val{N},tok) where N = begin
    return tok
end

# Note that `new_children` replaces `tr.children` as the idea is that they
# have been processed if they are provided as an argument
_call_userfunc(t::Transformer,tr::Tree; new_children = nothing) = begin
    children = new_children
    if new_children == nothing
        children = tr.children
    end
    # println("Processing $(tr.data)")
    return transformer_func(t,Val{Symbol(tr.data)}(),tr._meta,children)
end

_call_userfunc_token(t::Transformer,token) = begin
    return token_func(t,Val{Symbol(token.type_)}(),token)
end

# No need for a channel here as we just collect the results anyway.
# We annotate the return type as otherwise Julia will assume the type
# is the type of the first returned value.
_transform_children(t::Transformer,children)::Array{Any} = begin
    map(children) do c
        try
            if c isa Tree
                _transform_tree(t,c)
            elseif visit_tokens(t) && c isa Token
                _call_userfunc_token(t,c)
            else
                c
            end
        catch e
            # For production, drop the error...
            # println("Warning: discarding $e")
            rethrow(e)
        end
    end
end

_transform_tree(t::Transformer,tree) = begin
    children = _transform_children(t,tree.children)
    #println("Children were $(tree.children), now $children")
    return _call_userfunc(t,tree,new_children=children)
end

"""
    transform(tr::Transformer,tree)

Transform parse tree `tree` according to rules defined for type `tr`.
Typically `tr` is a subtype of `Transformer`.
"""
transform(tr::Transformer,tree) = _transform_tree(tr,tree)

Base.:*(t1::Transformer,t2::Transformer) = TransformerChain([t1,t2])

__default__(tr::Transformer,data,children,meta) = begin
    #println("Warning: calling default transformer for $data and $tr")
    Tree(data,children,meta)
end

# Weird introspective decorator magic skipped

struct TransformerChain
    transformers::Array{Transformer}
end

TransformerChain(transf...) = TransformerChain(transf)

Base.:*(tc::TransformerChain,tc2::TransformerChain) = begin
    new_transformers = tc.transformers
    append!(new_transformers,tc2.transformers)   #TODO: Check we have a nice flat list
    TransformerChain(new_transformers)
end

Base.:*(tc::TransformerChain,t::Transformer) = begin
    new_transformers = tc.transformers
    push!(new_transformers,t)
    TransformerChain(new_transformers)
end

transform(tc::TransformerChain,tree) = begin
    for t in tc.transformers
        tree = transform(t,tree)
    end
    return tree
end

_transform_tree(tip::Transformer_InPlace,tree) = begin
    _call_userfunc(tip,tree)
end


_transform_tree(tipr::Transformer_InPlaceRecursive,tree) = begin
    tree.children = _transform_children(tipr,tree.children)
    return _call_userfunc(tipr,tree)
end

transform(tip::Transformer_InPlace, tree) = begin
    for subtree in tree
        subtree.children = _transform_children(tip,subtree.children)
    end
    return _transform_tree(tip,tree)
end

struct Transformer_NonRecursive <: Transformer end

visit_tokens(t::Transformer_NonRecursive) = false

transform(tnr::Transformer_NonRecursive, tree) = begin
    rev_postfix = []
    q = Union{Token,Tree}[tree]
    while !isempty(q)
        t = pop!(q)
        push!(rev_postfix,t)
        if t isa Tree
            append!(q,t.children)
        end
    end
    stack = []
    for x in reverse(rev_postfix)
        if x isa Tree
            size = length(x.children)
            if size > 0
                args = stack[end-size+1:end]
                stack = stack[1:end-size]
            else
                args = []
            end
            push!(stack,_call_userfunc(tnr,x,new_children=args))
        else
            push!(stack,x)
        end
    end
    t = stack[]
    return t
end

#== Visitors ==#

abstract type VisitorBase end

"""
    Visitor

User-defined methods called with an instance of a subtype of Visitor will visit
each node of the parse tree in top-down order without altering it or returning 
a transformed tree.
"""
abstract type Visitor <: VisitorBase end

"""
    When called by `visit`, subtypes of `Visitor_Recursive` visit nodes of the parse tree
    in bottom-up order. 
"""
abstract type Visitor_Recursive <: VisitorBase end

_call_userfunc(v::VisitorBase,tree) = transformer_func(v,Val{Symbol(tree.data)}(),Meta(),tree)

transformer_func(v::VisitorBase,::Val,tree) = tree
transformer_func(v::VisitorBase,::Val,::Meta,tree) = tree

"""
    visit(v::Visitor,tree)

Visit each node of `tree`, calling methods defined for `v` on each node.
"""
visit(v::Visitor,tree) = begin
    for subtree in tree
        _call_userfunc(v,subtree)
    end
    return tree
end

visit_topdown(v::Visitor,tree) = begin
    for subtree in iter_subtrees_topdown(tree)
        _call_userfunc(v,subtree)
    end
    return tree
end

visit(v::Visitor_Recursive,tree) = begin
    for child in tree.children
        if child isa Tree
            visit(v,child)
        end
    end
    _call_userfunc(v,tree)
    return tree
end

visit_topdown(v::Visitor_Recursive,tree) = begin
    _call_userfunc(tree)
    for child in tree.children
        if child isa Tree
            visit_topdown(v,child)
        end
    end
    return tree
end

"""
    Subtypes of the `Interpreter` type do not automatically visit children
    of the parse tree node when called by `visit`.
"""
abstract type Interpreter end

"""
    transformer_func(i::Interpreter,::Val,meta,tree)

Default interpreter visitor does not automatically visit children
"""
transformer_func(i::Interpreter,::Val,meta,tree) = visit_children(i,tree)

visit(inter::Interpreter,tree) = begin
    transformer_func(inter,Val{Symbol(tree.data)}(),Meta(),tree)
end

visit_children(inter::Interpreter,tree) = begin
    map(tree.children) do child
        if typeof(child) == Tree
            visit(inter,child)
        else
            child
        end
    end
end

abstract type CollapseAmbiguities <: Transformer end

@rule _ambig(c::CollapseAmbiguities,options) = sum(options,[])

__default__(c::CollapseAmbiguities,data,children_lists,meta) = begin
    [Tree(data,children,meta) for children in combine_alternatives(children_lists)]
end

__default_token__(c::CollapseAmbiguities,t) = [t]
    
