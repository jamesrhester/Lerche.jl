"""Visits the tree recursively, starting with the leaves and finally the root (bottom-up)

    Calls its methods (provided by user) according to tree.data
    The returned value replaces the old one in the structure.

    Can be used to implement map or reduce.
"""
abstract type Transformer end

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

abstract type Transformer_InPlace <: Transformer end
abstract type Transformer_InPlaceRecursive <: Transformer end

_call_userfunc(t::Transformer,tr::Tree; new_children = nothing) = begin
    children = new_children
    if new_children == nothing
        children = tr.children
    end
    if have_method(t,tr.data)
        f = tr.data
    else
        return __default__(t,tr.data,children,tr._meta)
    end
    if is_inline(t,f)
        return get_method(t,f)(t,children...)
    else
        return get_method(t,f)(t,children)
    end
    
end

_transform_children(t::Transformer,children) = Channel() do chan
    for c in children
        try
            if c isa Tree
                put!(chan,_transform_tree(t,c))
            else
                put!(chan,c)
            end
        catch e
            # For production, drop the error...
            # println("Warning: discarding $e")
            rethrow(e)
        end
    end
end

_transform_tree(t::Transformer,tree) = begin
    children = collect(_transform_children(t,tree.children))
    #println("Children were $(tree.children), now $children")
    return _call_userfunc(t,tree,new_children=children)
end

# This is overridden by subtypes
transform(tr::Transformer,tree) = _transform_tree(tr,tree)

Base.:*(t1::Transformer,t2::Transformer) = TransformerChain([t1,t2])

__default__(tr::Transformer,data,children,meta) = begin
    #println("Warning: calling default transformer for $data")
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
    tree.children = collect(_transform_children(tipr,tree.children))
    return _call_userfunc(tipr,tree)
end

transform(tip::Transformer_InPlace, tree) = begin
    for subtree in collect(iter_subtrees(tree))
        subtree.children = collect(_transform_children(tip,subtree.children))
    end
    return _transform_tree(tip,tree)
end

#== Visitors ==#

abstract type VisitorBase end

abstract type Visitor <: VisitorBase end
abstract type Visitor_Recursive <: VisitorBase end

_call_userfunc(v::VisitorBase,tree) = begin
    if have_method(v,tree.data)
        get_method(v,tree.data)(v,tree)
    else
        __default__(v,tree)
    end
end

__default__(v::VisitorBase,tree) = tree

visit(v::Visitor,tree) = begin
    for subtree in iter_subtrees(tree)
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
    if have_method(v,tree.data)
        get_method(v,tree.data)(v,tree)
    end
    return tree
end

# Interpreters do not automatically visit children

abstract type Interpreter end

visit(inter::Interpreter,tree) = begin
    if have_method(inter,tree.data)
        get_method(inter,tree.data)(inter,tree)
    else
        return visit_children(inter,tree)
    end
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
