# What is Meta? A place to store extra information. All of
# the extra information that I could find is here
mutable struct Meta
    rule
    line::Int64
    column::Int64
    end_line::Int64
    end_column::Int64
    start_pos::Int64
    end_pos::Int64
    empty::Bool
    Meta() = new()
end

# Note that a Python slotted tree is just a tree with
# restricted possible fields, like a Julia struct.
"""
The main tree class.

    Creates a new tree, and stores "data" and "children" in attributes of the same name.
    Trees can be hashed and compared.

    Parameters:
        data: The name of the rule or alias
        children: List of matched sub-rules and terminals
        meta: Line & Column numbers (if ``propagate_positions`` is enabled).
            meta attributes: line, column, start_pos, end_line, end_column, end_pos
"""
mutable struct Tree
    data
    children
    _meta::Meta
end

Tree(data,children) = Tree(data,children,Meta())
Tree(data,children,n::Nothing) = Tree(data,children)

meta(t::Tree) = t._meta

_pretty_label(t::Tree) = t.data

pretty(t::Tree,level,indent_str::String) = begin
    if length(t.children) == 1 && !(typeof(t.children[1]) == Tree)
        return [repeat(indent_str,level), _pretty_label(t), "\t", "$(t.children[1])", "\n"]
    end

    l = [ repeat(indent_str,level), _pretty_label(t), "\n" ]
    for n in t.children
        if typeof(n) == Tree
            append!(l, pretty(n,level+1, indent_str))
        else
            append!(l, [ repeat(indent_str,level+1), "$n", "\n" ])
        end
    end
    return l
end

pretty(t::Tree; indent_str = "  ") = join(pretty(t,0, indent_str),"")

#Base.show(io::IO,t::Tree) = println(io,pretty(t))
Base.show(io::IO,t::Tree) = println(io,"Tree($(t.data), $(t.children))")
"""
Expand children at the given indices. Rewritten for Julia as we
can't expand inline.
"""
expand_kids_by_index!(t::Tree, indices) = begin
    #println("Expanding $t and $indices")
    new_children = []
    for (i,c) in enumerate(t.children)
        if i in indices
            append!(new_children,c.children)
        else
            push!(new_children,c)
        end
    end
    t.children = new_children
    #println("Returning: $t")
end

Base.:(==)(t1::Tree,t2::Tree) = begin
    return t1.data == t2.data && t1.children == t2.children
end

# When comparing a tree with something else, we compare only
# on data (as a string) and children. This is mainly for testing.
#==Base.:(==)(t1::Tree,t2::Array) = begin
    if length(t2) != 2
        println("Not equal: length != 2")
        return false
    end
    if t2[1] != t1.data
        println("Not equal: $(t2[1]) != $(t1.data)")
        return false
    end
    if length(t2[2]) != length(t1.children)
        println("Not equal: child length differs")
        return false
    end
    for (f,s) in zip(t1.children,t2[2])
        if f != s
            println("Not equal: $f != $s")
            return false
        end
    end
    return true
end
==#

Base.hash(t1::Tree,h::UInt) = begin
    h = hash(t1.data,h)
    for t in t1.children
        h = hash(t, h)
    end
    return h
end

find_pred(t::Tree,pred) = begin
    Iterators.filter(pred, t)
end

find_data(t::Tree,data) = begin
    find_pred(t, x -> x.data == data)
end

# Test only non-tree values for `pred`
scan_values(t::Tree,pred) = begin
    result = Any[]
    for c in t.children
        if typeof(c) == Tree
            append!(result,scan_values(c,pred))
        else
            if pred(c)
                push!(result,c)
            end
        end
    end
    return result
end

#==
scan_values(t::Tree,pred) = Channel() do val_chan
    begin
        for c in t.children
            if typeof(c) == Tree
                for t in scan_values(c,pred)
                    put!(val_chan,t)
                end
            else
                if pred(c)
                    put!(val_chan,c)
                end
            end
        end
    end
end
==#

Base.iterate(t::Tree) = begin
    visited = Set()
    q = [t]
    l = []
    while !isempty(q)
        subtree = pop!(q)
        push!(l,subtree)
        if objectid(subtree) in visited
            continue
        end
        push!(visited,objectid(subtree))
        append!(q,[c for c in subtree.children if typeof(c) == Tree])
    end
    seen = Set()
    idx = length(l)
    if idx == 0 return nothing end
    push!(seen,objectid(l[idx]))
    return l[idx],(seen,l,idx)
end

# `s` is a tuple `(seen,l,idx)` where `idx` is the most
# recently returned index.
Base.iterate(t::Tree,s) = begin
    seen,l,idx = s
    idx = idx -1
    while idx > 0 && objectid(l[idx]) in seen
        idx = idx - 1
    end
    if idx == 0 return nothing end
    push!(seen,objectid(l[idx]))
    return l[idx],(seen,l,idx)
end

tree_set(t::Tree,data,children) = begin
    t.data = data
    t.children = children
end

